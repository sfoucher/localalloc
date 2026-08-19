#' Maximum Capture Problem (MAXCAP)
#'
#' Selects at most `p_facilities` candidate sites to maximize the weighted
#' demand captured from a competitor (`existing_sites`), assuming each
#' demand point currently patronizes its nearest existing site and
#' switches to any newly opened candidate that is closer.
#'
#' @details
#' Reduced formulation: omits the tie-breaking mechanism between a
#' candidate and the incumbent competitor (assumes no exact-distance
#' ties).
#' \deqn{\text{Maximize } z = \sum_{i \in I} w_i Y_i}
#' \deqn{\text{s.t. } Y_i \leq \sum_{j \in p_i} X_j, \; \forall i \qquad \sum_{j \in J} X_j \leq P}
#' \deqn{X_j, Y_i \in \{0,1\}}
#' where \eqn{w_i} = `demand_weight`, \eqn{P} = `p_facilities`, and
#' \eqn{p_i} is the set of candidate sites closer to \eqn{i} than its
#' nearest competitor.
#'
#' @inheritParams lscp
#' @param demand_weight character or NULL. Weight column in `demand` (e.g.
#'   population). This drives the objective -- captured demand is weighted
#'   by this column. Defaults to 1 if NULL.
#' @param existing_sites sf POINT. The competitor's sites (required).
#' @param existing_sites_id character. Unique id column in `existing_sites`.
#' @param matrix_OD_existing_site data.frame. Required.
#' @param p_facilities integer. Maximum number of new facilities to open
#'   (the budget constraint is `<=`, not `=`, per eq. 2.35).
#' @return An object of class `llocalocal_result`.
#' @export
maxcap <- function(demand, demand_id, demand_weight = NULL,
                    candidate, candidate_id,
                    existing_sites, existing_sites_id,
                    existing_sites_weight = NULL,
                    matrix_OD_candidates,
                    matrix_OD_candidates_from_id = "from_id",
                    matrix_OD_candidates_to_id = "to_id",
                    matrix_OD_candidates_dist = "distance",
                    matrix_OD_existing_site,
                    matrix_OD_existing_site_from_id = "from_id",
                    matrix_OD_existing_site_to_id = "to_id",
                    matrix_OD_existing_site_dist = "distance",
                    cutoff_distance = NULL,
                    p_facilities,
                    solver = "highs") {
  t0 <- Sys.time()

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)
  if (is.null(existing_sites))
    stop("`existing_sites` is required for maxcap() (they are the competitor sites).")
  validate_sf(existing_sites, "existing_sites", existing_sites_id)

  collision <- intersect(as.character(candidate[[candidate_id]]),
                         as.character(existing_sites[[existing_sites_id]]))
  if (length(collision) > 0)
    stop(sprintf("Ids shared between `candidate` and `existing_sites`: %s",
                 paste(collision, collapse = ", ")))

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
  validate_cost_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                       matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                       name = "matrix_OD_existing_site")

  demand <- set_weights(demand, demand_id, demand_weight, "demand")
  weight_col <- if (is.null(demand_weight)) "weight" else demand_weight
  a <- as.numeric(demand[[weight_col]])

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  ids_exist  <- as.character(existing_sites[[existing_sites_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)

  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                cutoff_distance, ids_from = ids_demand, ids_to = ids_cand)
  cost_mat_cand <- replace_inf(cost_mat_cand)

  cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                 matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                 cutoff_distance, ids_from = ids_demand, ids_to = ids_exist)
  cost_mat_exist <- replace_inf(cost_mat_exist)

  baseline <- derive_competitor_baseline(cost_mat_exist)

  bij <- matrix(0L, nrow = n_cli, ncol = n_fac, dimnames = list(ids_demand, ids_cand))
  for (jj in seq_len(n_fac))
    bij[, jj] <- as.integer(cost_mat_cand[, jj] < baseline)

  message("MAXCAP | building sparse MIP...")
  cov <- which(bij == 1, arr.ind = TRUE)
  n_vars <- n_cli + n_fac

  L <- c(a, rep(0, n_fac))
  A_p   <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = n_cli + seq_len(n_fac), x = 1,
                                dims = c(1, n_vars))
  A_cov <- Matrix::sparseMatrix(
    i = c(seq_len(n_cli), cov[, 1]), j = c(seq_len(n_cli), n_cli + cov[, 2]),
    x = c(rep(1, n_cli), rep(-1, nrow(cov))), dims = c(n_cli, n_vars))
  A <- rbind(A_p, A_cov)
  dir <- c("<=", rep("<=", n_cli))
  rhs <- c(p_facilities, rep(0, n_cli))

  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("MAXCAP | solving | %d demand points | %d candidates (<= %d) | solver: %s",
                  n_cli, n_fac, p_facilities, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  Y_vals <- result$solution[seq_len(n_cli)]
  X_vals <- result$solution[(n_cli + 1):(n_cli + n_fac)]
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  covered_demand <- sum(a[round(Y_vals) == 1])

  build_result(
    model_type = "maxcap", solver_status = result$status, sf_selected = sf_selected,
    covered_demand = covered_demand, n_open = length(ids_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
