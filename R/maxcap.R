#' Maximum Capture Problem (MAXCAP)
#'
#' Selects at most `p_facilities` candidate sites to maximize the weighted
#' demand captured from a competitor (`existing_sites`), assuming each
#' demand point currently patronizes its nearest existing site and
#' switches to any newly opened candidate that is closer.
#'
#' @inheritParams lscp
#' @param existing_sites sf POINT. The competitor's sites (required).
#' @param existing_sites_id character. Unique id column in `existing_sites`.
#' @param matrix_OD_existing_site data.frame. Required.
#' @param p_facilities integer. Maximum number of new facilities to open
#'   (the budget constraint is `<=`, not `=`, per eq. 2.35).
#' @return An object of class `llocalocal_result`.
#' @export
maxcap <- function(demand, demand_id, demand_weight = NULL,
                    candidate, candidate_id, candidate_weight = NULL,
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
                    cutoff_distance = 1000,
                    p_facilities,
                    solver = "glpk") {

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
                                cutoff_distance)
  cost_mat_cand <- replace_inf(cost_mat_cand[ids_demand, ids_cand, drop = FALSE])

  cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                 matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                 cutoff_distance)
  cost_mat_exist <- replace_inf(cost_mat_exist[ids_demand, ids_exist, drop = FALSE])

  baseline <- derive_competitor_baseline(cost_mat_exist)

  bij <- matrix(0L, nrow = n_cli, ncol = n_fac, dimnames = list(ids_demand, ids_cand))
  for (jj in seq_len(n_fac))
    bij[, jj] <- as.integer(cost_mat_cand[, jj] < baseline)

  model <- ompr::MIPModel() |>
    ompr::add_variable(X[j], j = 1:n_fac, type = "binary") |>
    ompr::add_variable(Y[i], i = 1:n_cli, type = "binary") |>
    ompr::set_objective(ompr::sum_expr(a[i] * Y[i], i = 1:n_cli), sense = "max") |>
    ompr::add_constraint(ompr::sum_expr(X[j], j = 1:n_fac) <= p_facilities) |>
    ompr::add_constraint(Y[i] <= ompr::sum_expr(bij[i, j] * X[j], j = 1:n_fac), i = 1:n_cli)

  message(sprintf("MAXCAP | %d demand points | %d candidates (<= %d) | solver: %s",
                  n_cli, n_fac, p_facilities, solver))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf("Solver '%s' failed: %s", solver, e$message))
  )
  if (result$status != "success")
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- ompr::get_solution(result, X[j])$value
  Y_vals <- ompr::get_solution(result, Y[i])$value
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  covered_demand <- sum(a[round(Y_vals) == 1])

  build_result(
    model_type = "maxcap", solver_status = result$status, sf_selected = sf_selected,
    covered_demand = covered_demand, n_open = length(ids_selected), n_demand = n_cli
  )
}
