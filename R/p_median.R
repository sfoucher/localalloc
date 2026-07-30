.assignment_model <- function(demand, demand_id, demand_weight,
                               candidate, candidate_id, candidate_weight,
                               existing_sites, existing_sites_id, existing_sites_weight,
                               matrix_OD_candidates, matrix_OD_candidates_from_id,
                               matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                               matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                               matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                               cutoff_distance, p_facilities, solver,
                               sense, model_type) {

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

  if (!is.numeric(cutoff_distance) || cutoff_distance <= 0)
    stop("`cutoff_distance` must be a positive number.")
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
  cost_mat <- replace_inf(cost_mat_all)

  model <- ompr::MIPModel() |>
    ompr::add_variable(X[j], j = 1:n_all_fac, type = "binary") |>
    ompr::add_variable(Y[i, j], i = 1:n_cli, j = 1:n_all_fac, type = "binary") |>
    ompr::set_objective(
      ompr::sum_expr(weight_demand[i] * cost_mat[i, j] * Y[i, j], i = 1:n_cli, j = 1:n_all_fac),
      sense = sense
    ) |>
    ompr::add_constraint(ompr::sum_expr(X[j], j = 1:n_fac) == p_facilities) |>
    ompr::add_constraint(ompr::sum_expr(Y[i, j], j = 1:n_all_fac) == 1, i = 1:n_cli) |>
    ompr::add_constraint(Y[i, j] <= X[j], i = 1:n_cli, j = 1:n_all_fac)

  if (has_existing)
    model <- ompr::add_constraint(model, X[j] == 1, j = (n_fac + 1):n_all_fac)

  message(sprintf("%s | %d demand points | %d candidates (p=%d)%s | solver: %s",
                  toupper(model_type), n_cli, n_fac, p_facilities,
                  if (has_existing) sprintf(" | %d existing (forced)", n_exist) else "",
                  solver))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf("Solver '%s' failed: %s", solver, e$message))
  )
  if (result$status != "success")
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  Y_vals <- ompr::get_solution(result, Y[i, j])
  X_vals <- ompr::get_solution(result, X[j])$value
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
    n_demand = n_cli
  )
}

#' P-Median Problem
#'
#' Selects exactly `p_facilities` candidate sites to minimize the total
#' weighted distance between each demand point and its nearest open
#' facility. Sites in `existing_sites` are forced open from the start
#' ("Required Facilities").
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
                      p_facilities,
                      solver = "glpk") {
  .assignment_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id, candidate_weight,
    existing_sites, existing_sites_id, existing_sites_weight,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    matrix_OD_existing_site, matrix_OD_existing_site_from_id,
    matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
    cutoff_distance, p_facilities, solver,
    sense = "min", model_type = "p_median"
  )
}
