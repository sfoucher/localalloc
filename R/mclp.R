#' Maximal Covering Location Problem (MCLP)
#'
#' Selects up to `p_facilities` candidate sites to maximize the weighted
#' demand covered within `service_radius`. Unlike [lscp()], full coverage
#' of every demand point is not required -- demand that can't be reached
#' by any candidate within the radius is simply left uncovered, which is
#' the point of "maximal" coverage rather than total coverage.
#'
#' @inheritParams lscp
#' @param p_facilities integer. Number of facilities to open.
#' @return An object of class `llocalocal_result`.
#' @export
mclp <- function(demand, demand_id, demand_weight = NULL,
                  candidate, candidate_id, candidate_weight = NULL,
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
                  cutoff_distance = 1000,
                  service_radius,
                  p_facilities,
                  solver = "glpk") {

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)

  if (!is.numeric(service_radius) || length(service_radius) != 1 || service_radius <= 0)
    stop("`service_radius` must be a single positive number.")
  if (!is.numeric(p_facilities) || p_facilities < 1)
    stop("`p_facilities` must be an integer >= 1.")
  p_facilities <- as.integer(p_facilities)

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")

  demand <- set_weights(demand, demand_id, demand_weight, "demand")
  weight_col <- if (is.null(demand_weight)) "weight" else demand_weight
  a <- as.numeric(demand[[weight_col]])

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

  cost_mat <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                           matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                           cutoff_distance)
  cost_mat <- cost_mat[ids_demand, ids_cand, drop = FALSE]
  bij <- make_coverage_matrix(cost_mat, service_radius)

  model <- ompr::MIPModel() |>
    ompr::add_variable(X[j], j = 1:n_fac, type = "binary") |>
    ompr::add_variable(Y[i], i = 1:n_cli, type = "binary") |>
    ompr::set_objective(ompr::sum_expr(a[i] * Y[i], i = 1:n_cli), sense = "max") |>
    ompr::add_constraint(ompr::sum_expr(X[j], j = 1:n_fac) == p_facilities) |>
    ompr::add_constraint(Y[i] <= ompr::sum_expr(bij[i, j] * X[j], j = 1:n_fac), i = 1:n_cli)

  message(sprintf("MCLP | %d demand points | %d candidates | radius = %g | p = %d | solver: %s",
                  n_cli, n_fac, service_radius, p_facilities, solver))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf("Solver '%s' failed: %s", solver, e$message))
  )
  if (result$status != "optimal")
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- ompr::get_solution(result, X[j])$value
  Y_vals <- ompr::get_solution(result, Y[i])$value
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  covered_demand <- sum(a[round(Y_vals) == 1])

  build_result(
    model_type = "mclp", solver_status = result$status, sf_selected = sf_selected,
    covered_demand = covered_demand, n_open = length(ids_selected), n_demand = n_cli
  )
}
