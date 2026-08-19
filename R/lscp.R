#' Location Set Covering Problem (LSCP)
#'
#' Finds the minimum number of facilities to open so that every demand
#' point is covered by at least one facility within `service_radius`.
#'
#' @details
#' \deqn{\text{Minimize } z = \sum_{j=1}^{m} X_j}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} b_{ij} X_j \geq 1, \; \forall i \qquad X_j \in \{0,1\}, \; \forall j}
#' where \eqn{b_{ij} = 1} if site \eqn{j} covers demand \eqn{i} (i.e.
#' \eqn{d_{ij} \leq S}, with \eqn{S} = `service_radius`), 0 otherwise.
#'
#' @param demand sf POINT. Demand points.
#' @param demand_id character. Unique id column in `demand`.
#' @param demand_weight character or NULL. Weight column in `demand`.
#'   Unused by LSCP's objective (full coverage is required regardless of
#'   weight) -- not validated, since it has no effect.
#' @param candidate sf POINT. Candidate facility sites.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param existing_sites sf POINT or NULL. Facilities already open, forced
#'   into the solution.
#' @param existing_sites_id character or NULL.
#' @param existing_sites_weight character or NULL. Unused by LSCP.
#' @param matrix_OD_candidates data.frame. Long OD table demand-to-candidate.
#' @param matrix_OD_candidates_from_id character.
#' @param matrix_OD_candidates_to_id character.
#' @param matrix_OD_candidates_dist character.
#' @param matrix_OD_existing_site data.frame or NULL.
#' @param matrix_OD_existing_site_from_id character or NULL.
#' @param matrix_OD_existing_site_to_id character or NULL.
#' @param matrix_OD_existing_site_dist character or NULL.
#' @param cutoff_distance numeric or NULL. Pairs beyond this distance are
#'   dropped. `NULL` (default) means no cutoff.
#' @param service_radius numeric. Maximum acceptable distance.
#' @param solver character. `"highs"` (default) or `"glpk"`.
#' @return An object of class `llocalocal_result`.
#' @export
lscp <- function(demand, demand_id, demand_weight = NULL,
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
                  service_radius,
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
    collision <- intersect(
      as.character(candidate[[candidate_id]]),
      as.character(existing_sites[[existing_sites_id]])
    )
    if (length(collision) > 0) {
      warning(sprintf(
        "%d id(s) appear in both `candidate` and `existing_sites`; excluding them from `candidate` since they are already open: %s",
        length(collision), paste(collision, collapse = ", ")
      ))
      candidate <- candidate[!as.character(candidate[[candidate_id]]) %in% collision, , drop = FALSE]
    }
  }

  if (!is.numeric(service_radius) || length(service_radius) != 1 || service_radius <= 0)
    stop("`service_radius` must be a single positive number.")
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
  n_cli <- length(ids_demand)
  n_fac <- length(ids_cand)

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

  bij <- make_coverage_matrix(cost_mat_all, service_radius)
  uncovered <- which(rowSums(bij) == 0)
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) cannot be covered by any facility at service_radius = %g: %s",
      length(uncovered), service_radius, paste(ids_demand[uncovered], collapse = ", ")
    ))

  message("LSCP | building sparse MIP...")
  cov <- which(bij == 1, arr.ind = TRUE)
  idx_i <- cov[, 1]; idx_j <- cov[, 2]
  n_vars <- n_all_fac

  L <- rep(1, n_vars)
  A <- Matrix::sparseMatrix(i = idx_i, j = idx_j, x = 1, dims = c(n_cli, n_vars))
  dir <- rep(">=", n_cli)
  rhs <- rep(1, n_cli)

  if (has_existing) {
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("LSCP | solving | %d demand points | %d candidates | radius = %g | solver: %s",
                   n_cli, n_fac, service_radius, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "min", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- result$solution
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_all_fac[selected_j]
  ids_selected_cand <- intersect(ids_selected, ids_cand)

  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected_cand, , drop = FALSE]

  build_result(
    model_type = "lscp", solver_status = result$status, sf_selected = sf_selected,
    n_open = length(ids_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
