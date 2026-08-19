#' P-Center Problem
#'
#' Selects exactly `p_facilities` candidate sites to minimize the maximum
#' distance between any demand point and its assigned facility (minimax).
#'
#' @details
#' \deqn{\text{Minimize } z}
#' \deqn{\text{s.t. } z \geq d_{ij} Y_{ij}, \; \forall i,j \qquad \sum_{j=1}^{m} Y_{ij} = 1, \; \forall i}
#' \deqn{Y_{ij} \leq X_j, \; \forall i,j \qquad \sum_{j=1}^{m} X_j = p \qquad X_j, Y_{ij} \in \{0,1\}}
#' where \eqn{d_{ij}} = distance (OD matrix), \eqn{p} = `p_facilities`,
#' \eqn{z} is the model's `Z` variable (maximum distance).
#'
#' @inheritParams lscp
#' @param demand_weight character or NULL. Weight column in `demand`.
#'   Unused by P-Center's objective (the minimax criterion doesn't weight
#'   demand) -- not validated, since it has no effect.
#' @param p_facilities integer. Number of facilities to open.
#' @return An object of class `llocalocal_result`.
#' @export
p_center <- function(demand, demand_id, demand_weight = NULL,
                      candidate, candidate_id,
                      existing_sites = NULL, existing_sites_id = NULL,
                      existing_sites_weight = NULL,
                      matrix_OD_candidates,
                      matrix_OD_candidates_from_id = "from_id",
                      matrix_OD_candidates_to_id = "to_id",
                      matrix_OD_candidates_dist = "distance",
                      matrix_OD_existing_site = NULL,
                      matrix_OD_existing_site_from_id = NULL,
                      matrix_OD_existing_site_to_id = NULL,
                      matrix_OD_existing_site_dist = NULL,
                      cutoff_distance = NULL,
                      p_facilities,
                      solver = "highs") {
  t0 <- Sys.time()

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)

  has_existing <- !is.null(existing_sites)
  if (has_existing) {
    if (is.null(existing_sites_id))
      stop("`existing_sites_id` is required when `existing_sites` is supplied.")
    validate_sf(existing_sites, "existing_sites", existing_sites_id)
    if (is.null(matrix_OD_existing_site))
      stop("`matrix_OD_existing_site` is required when `existing_sites` is supplied.")
    collision <- intersect(as.character(candidate[[candidate_id]]),
                           as.character(existing_sites[[existing_sites_id]]))
    if (length(collision) > 0) {
      warning(sprintf(
        "%d id(s) appear in both `candidate` and `existing_sites`; excluding them from `candidate` since they are already open: %s",
        length(collision), paste(collision, collapse = ", ")
      ))
      candidate <- candidate[!as.character(candidate[[candidate_id]]) %in% collision, , drop = FALSE]
    }
  }

  if (!is.numeric(p_facilities) || p_facilities < 1)
    stop("`p_facilities` must be an integer >= 1.")
  p_facilities <- as.integer(p_facilities)

  if (is.null(cutoff_distance)) {
    cutoff_distance <- Inf
  } else if (!is.numeric(cutoff_distance) || cutoff_distance <= 0) {
    stop("`cutoff_distance` must be NULL (no cutoff) or a positive number.")
  }

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")
  if (has_existing)
    validate_cost_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                         matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                         name = "matrix_OD_existing_site")

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

  message(sprintf("P_CENTER | building cost matrix (%d demand points x %d candidates)...",
                  n_cli, n_fac))
  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                cutoff_distance, ids_from = ids_demand, ids_to = ids_cand)

  if (has_existing) {
    ids_exist <- as.character(existing_sites[[existing_sites_id]])
    n_exist <- length(ids_exist)
    cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                   matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                   cutoff_distance, ids_from = ids_demand, ids_to = ids_exist)
    ids_all_fac <- c(ids_cand, ids_exist)
    cost_mat_all <- cbind(cost_mat_cand, cost_mat_exist)
  } else {
    ids_all_fac <- ids_cand
    cost_mat_all <- cost_mat_cand
  }
  n_all_fac <- length(ids_all_fac)
  cost_mat <- cost_mat_all

  uncovered <- ids_demand[!apply(is.finite(cost_mat), 1, any)]
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) have no candidate or existing site within `cutoff_distance` (%.3g): %s",
      length(uncovered), cutoff_distance, paste(uncovered, collapse = ", ")
    ))

  message("P_CENTER | building sparse MIP...")
  valid <- which(is.finite(cost_mat), arr.ind = TRUE)
  idx_i <- valid[, 1]; idx_j <- valid[, 2]
  n_y <- nrow(valid)
  z_col <- n_y + n_all_fac + 1
  n_vars <- z_col

  L <- c(rep(0, n_y + n_all_fac), 1)

  A_p      <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = n_y + seq_len(n_fac), x = 1,
                                   dims = c(1, n_vars))
  A_assign <- Matrix::sparseMatrix(i = idx_i, j = seq_len(n_y), x = 1,
                                   dims = c(n_cli, n_vars))
  A_link   <- Matrix::sparseMatrix(i = rep(seq_len(n_y), 2), j = c(seq_len(n_y), n_y + idx_j),
                                   x = c(rep(1, n_y), rep(-1, n_y)), dims = c(n_y, n_vars))
  A_minimax <- Matrix::sparseMatrix(
    i = c(idx_i, seq_len(n_cli)), j = c(seq_len(n_y), rep(z_col, n_cli)),
    x = c(cost_mat[cbind(idx_i, idx_j)], rep(-1, n_cli)), dims = c(n_cli, n_vars))
  A <- rbind(A_p, A_assign, A_link, A_minimax)
  dir <- c("==", rep("==", n_cli), rep("<=", n_y), rep("<=", n_cli))
  rhs <- c(p_facilities, rep(1, n_cli), rep(0, n_y), rep(0, n_cli))

  if (has_existing) {
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_y + n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  types <- c(rep("B", n_y + n_all_fac), "C")
  lower <- c(rep(0, n_y + n_all_fac), 0)
  upper <- c(rep(1, n_y + n_all_fac), Inf)

  message(sprintf("P_CENTER | solving | %d demand points | %d candidates (p=%d) | solver: %s",
                  n_cli, n_fac, p_facilities, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "min", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  Y_vals <- data.frame(i = idx_i, j = idx_j, value = result$solution[seq_len(n_y)])
  X_vals <- result$solution[(n_y + 1):(n_y + n_all_fac)]
  max_distance <- result$solution[z_col]
  selected_j <- which(round(X_vals[1:n_fac]) == 1)
  ids_selected <- ids_cand[selected_j]

  assignments <- extract_assignment(Y_vals, ids_demand, ids_all_fac, cost_mat)
  assignments$source <- ifelse(assignments$facility_id %in% ids_cand, "candidate", "existing")

  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]

  build_result(
    model_type = "p_center", solver_status = result$status, sf_selected = sf_selected,
    assignments = assignments, max_distance = max_distance,
    n_open = length(ids_selected) + if (has_existing) n_exist else 0L,
    n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
