.assignment_model <- function(demand, demand_id, demand_weight,
                               candidate, candidate_id,
                               existing_sites, existing_sites_id, existing_sites_weight,
                               matrix_OD_candidates, matrix_OD_candidates_from_id,
                               matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                               matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                               matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                               cutoff_distance, p_facilities, solver,
                               sense, model_type) {
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

  if (is.null(cutoff_distance)) {
    cutoff_distance <- Inf
  } else if (!is.numeric(cutoff_distance) || cutoff_distance <= 0) {
    stop("`cutoff_distance` must be NULL (no cutoff) or a positive number.")
  }
  if (!is.numeric(p_facilities) || p_facilities < 1)
    stop("`p_facilities` must be an integer >= 1.")
  p_facilities <- as.integer(p_facilities)

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")
  if (has_existing)
    validate_cost_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                         matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                         name = "matrix_OD_existing_site")

  demand <- set_weights(demand, demand_id, demand_weight, "demand")
  weight_col <- if (is.null(demand_weight)) "weight" else demand_weight
  weight_demand <- as.numeric(demand[[weight_col]])

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

  message(sprintf("%s | building cost matrix (%d demand points x %d candidates)...",
                  toupper(model_type), n_cli, n_fac))
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
    cost_mat <- cbind(cost_mat_cand, cost_mat_exist)
  } else {
    ids_all_fac <- ids_cand
    cost_mat <- cost_mat_cand
  }
  n_all_fac <- length(ids_all_fac)

  uncovered <- ids_demand[!apply(is.finite(cost_mat), 1, any)]
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) have no candidate or existing site within `cutoff_distance` (%.3g): %s",
      length(uncovered), cutoff_distance, paste(uncovered, collapse = ", ")
    ))

  message(sprintf("%s | building sparse MIP...", toupper(model_type)))
  valid <- which(is.finite(cost_mat), arr.ind = TRUE)
  idx_i <- valid[, 1]; idx_j <- valid[, 2]
  n_y <- nrow(valid)
  n_vars <- n_y + n_all_fac

  L <- c(weight_demand[idx_i] * cost_mat[cbind(idx_i, idx_j)], rep(0, n_all_fac))

  A_p      <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = n_y + seq_len(n_fac), x = 1,
                                   dims = c(1, n_vars))
  A_assign <- Matrix::sparseMatrix(i = idx_i, j = seq_len(n_y), x = 1,
                                   dims = c(n_cli, n_vars))
  A_link   <- Matrix::sparseMatrix(i = rep(seq_len(n_y), 2), j = c(seq_len(n_y), n_y + idx_j),
                                   x = c(rep(1, n_y), rep(-1, n_y)), dims = c(n_y, n_vars))
  A <- rbind(A_p, A_assign, A_link)
  dir <- c("==", rep("==", n_cli), rep("<=", n_y))
  rhs <- c(p_facilities, rep(1, n_cli), rep(0, n_y))

  if (has_existing) {
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_y + n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("%s | solving | %d demand points | %d candidates (p=%d)%s | solver: %s",
                  toupper(model_type), n_cli, n_fac, p_facilities,
                  if (has_existing) sprintf(" | %d existing (forced)", n_exist) else "",
                  solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = sense, solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  Y_vals <- data.frame(i = idx_i, j = idx_j, value = result$solution[seq_len(n_y)])
  X_vals <- result$solution[(n_y + 1):(n_y + n_all_fac)]
  selected_j <- which(round(X_vals[1:n_fac]) == 1)
  ids_selected <- ids_cand[selected_j]

  assignments <- extract_assignment(Y_vals, ids_demand, ids_all_fac, cost_mat)
  assignments$source <- ifelse(assignments$facility_id %in% ids_cand, "candidate", "existing")

  total_cost <- sum(weight_demand * ifelse(is.finite(assignments$distance), assignments$distance, 0),
                    na.rm = TRUE)

  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]

  build_result(
    model_type = model_type, solver_status = result$status, sf_selected = sf_selected,
    assignments = assignments, total_cost = total_cost,
    n_open = length(ids_selected) + if (has_existing) n_exist else 0L,
    n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

#' P-Median Problem
#'
#' Selects exactly `p_facilities` candidate sites to minimize the total
#' weighted distance between each demand point and its nearest open
#' facility. Sites in `existing_sites` are forced open from the start
#' ("Required Facilities").
#'
#' @details
#' \deqn{\text{Minimize } z = \sum_{i=1}^{n} \sum_{j=1}^{m} a_i d_{ij} Y_{ij}}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} Y_{ij} = 1, \; \forall i \qquad Y_{ij} \leq X_j, \; \forall i,j \qquad \sum_{j=1}^{m} X_j = p}
#' \deqn{X_j, Y_{ij} \in \{0,1\}}
#' where \eqn{a_i} = `demand_weight`, \eqn{d_{ij}} = distance (OD matrix),
#' \eqn{p} = `p_facilities`.
#'
#' @inheritParams lscp
#' @param demand_weight character or NULL. Weight column in `demand` (e.g.
#'   population). This is the primary driver of the objective -- candidates
#'   are chosen to minimize (p_median) or maximize (uflp) total weighted
#'   assigned distance. Defaults to 1 if NULL.
#' @param p_facilities integer. Number of new facilities to open.
#' @return An object of class `llocalocal_result`.
#' @export
p_median <- function(demand, demand_id, demand_weight = NULL,
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
  .assignment_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id,
    existing_sites, existing_sites_id, existing_sites_weight,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    matrix_OD_existing_site, matrix_OD_existing_site_from_id,
    matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
    cutoff_distance, p_facilities, solver,
    sense = "min", model_type = "p_median"
  )
}
