#' Location Set Covering Problem (LSCP)
#'
#' Finds the minimum number of facilities to open so that every demand
#' point is covered by at least one facility within `service_radius`.
#'
#' @param demand sf POINT. Demand points.
#' @param demand_id character. Unique id column in `demand`.
#' @param demand_weight character or NULL. Weight column in `demand`.
#'   Unused by LSCP's objective (full coverage is required regardless of
#'   weight) but validated for consistency with the other models.
#' @param candidate sf POINT. Candidate facility sites.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param candidate_weight character or NULL. Unused by LSCP.
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
#' @param cutoff_distance numeric. Pairs beyond this distance are dropped.
#' @param service_radius numeric. Maximum acceptable distance.
#' @param solver character. ROI solver, default `"glpk"`.
#' @return An object of class `llocalocal_result`.
#' @export
lscp <- function(demand, demand_id, demand_weight = NULL,
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
                  solver = "glpk") {

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
    if (length(collision) > 0)
      stop(sprintf("Ids shared between `candidate` and `existing_sites`: %s",
                    paste(collision, collapse = ", ")))
  }

  if (!is.numeric(service_radius) || length(service_radius) != 1 || service_radius <= 0)
    stop("`service_radius` must be a single positive number.")
  if (!is.numeric(cutoff_distance) || cutoff_distance <= 0)
    stop("`cutoff_distance` must be a positive number.")

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
                                cutoff_distance)
  cost_mat_cand <- cost_mat_cand[ids_demand, ids_cand, drop = FALSE]

  if (has_existing) {
    ids_exist <- as.character(existing_sites[[existing_sites_id]])
    n_exist <- length(ids_exist)
    cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                   matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                   cutoff_distance)
    cost_mat_exist <- cost_mat_exist[ids_demand, ids_exist, drop = FALSE]
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

  model <- ompr::MIPModel() |>
    ompr::add_variable(X[j], j = 1:n_all_fac, type = "binary") |>
    ompr::set_objective(ompr::sum_expr(X[j], j = 1:n_all_fac), sense = "min") |>
    ompr::add_constraint(ompr::sum_expr(bij[i, j] * X[j], j = 1:n_all_fac) >= 1, i = 1:n_cli)

  if (has_existing)
    model <- ompr::add_constraint(model, X[j] == 1, j = (n_fac + 1):n_all_fac)

  message(sprintf("LSCP | %d demand points | %d candidates | radius = %g | solver: %s",
                   n_cli, n_fac, service_radius, solver))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf("Solver '%s' failed: %s", solver, e$message))
  )
  if (result$status != "success")
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- ompr::get_solution(result, X[j])$value
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_all_fac[selected_j]
  ids_selected_cand <- intersect(ids_selected, ids_cand)

  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected_cand, , drop = FALSE]

  build_result(
    model_type = "lscp", solver_status = result$status, sf_selected = sf_selected,
    n_open = length(ids_selected), n_demand = n_cli
  )
}
