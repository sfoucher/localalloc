#' Maximal Covering Location Problem (MCLP)
#'
#' Selects up to `p_facilities` candidate sites to maximize the weighted
#' demand covered within `service_radius`. Unlike [lscp()], full coverage
#' of every demand point is not required -- demand that can't be reached
#' by any candidate within the radius is simply left uncovered, which is
#' the point of "maximal" coverage rather than total coverage.
#' `existing_sites`, if supplied, are treated as already open: they
#' contribute coverage for free and don't consume the `p_facilities`
#' budget (same Required Facilities semantics as [lscp()]/[p_median()]/
#' [p_center()]).
#'
#' @details
#' \deqn{\text{Maximize } z = \sum_{i=1}^{n} a_i Y_i}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} b_{ij} X_j \geq Y_i, \; \forall i \qquad \sum_{j=1}^{m} X_j = p}
#' \deqn{X_j, Y_i \in \{0,1\}}
#' where \eqn{a_i} = `demand_weight`, \eqn{b_{ij} = 1} if \eqn{d_{ij} \leq S}
#' (`service_radius`), \eqn{p} = `p_facilities`.
#'
#' @inheritParams lscp
#' @param demand_weight character or NULL. Weight column in `demand` (e.g.
#'   population). This is the primary driver of MCLP's objective --
#'   candidates are chosen to maximize total weighted covered demand.
#'   Defaults to 1 if NULL.
#' @param service_radius numeric. Maximum acceptable distance.
#' @param p_facilities integer. Number of *new* facilities to open (the
#'   budget applies only to `candidate`, not to forced-open
#'   `existing_sites`).
#' @return An object of class `llocalocal_result`.
#' @export
mclp <- function(demand, demand_id, demand_weight = NULL,
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

  if (!is.numeric(service_radius) || length(service_radius) != 1 || service_radius <= 0)
    stop("`service_radius` must be a single positive number.")
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
  a <- as.numeric(demand[[weight_col]])

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

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

  message("MCLP | building sparse MIP...")
  cov <- which(bij == 1, arr.ind = TRUE)
  n_vars <- n_cli + n_all_fac

  L <- c(a, rep(0, n_all_fac))

  A_p    <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = n_cli + seq_len(n_fac), x = 1,
                                 dims = c(1, n_vars))
  A_cov  <- Matrix::sparseMatrix(
    i = c(seq_len(n_cli), cov[, 1]), j = c(seq_len(n_cli), n_cli + cov[, 2]),
    x = c(rep(1, n_cli), rep(-1, nrow(cov))), dims = c(n_cli, n_vars))
  A <- rbind(A_p, A_cov)
  dir <- c("==", rep("<=", n_cli))
  rhs <- c(p_facilities, rep(0, n_cli))

  if (has_existing) {
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_cli + n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("MCLP | solving | %d demand points | %d candidates | radius = %g | p = %d%s | solver: %s",
                  n_cli, n_fac, service_radius, p_facilities,
                  if (has_existing) sprintf(" | %d existing (forced)", n_exist) else "",
                  solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  Y_vals <- result$solution[seq_len(n_cli)]
  X_vals <- result$solution[(n_cli + 1):(n_cli + n_all_fac)]
  selected_j <- which(round(X_vals[1:n_fac]) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  covered_demand <- sum(a[round(Y_vals) == 1])

  build_result(
    model_type = "mclp", solver_status = result$status, sf_selected = sf_selected,
    covered_demand = covered_demand,
    n_open = length(ids_selected) + if (has_existing) n_exist else 0L,
    n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
