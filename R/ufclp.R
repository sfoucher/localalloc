.fixed_charge_model <- function(demand, demand_id, demand_weight,
                                 candidate, candidate_id, candidate_weight,
                                 matrix_OD_candidates, matrix_OD_candidates_from_id,
                                 matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                 cutoff_distance, candidate_fixed_cost, candidate_capacity,
                                 transport_cost_rate, solver, model_type) {

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)
  validate_fixed_cost(candidate, candidate_fixed_cost)
  has_capacity <- !is.null(candidate_capacity)
  if (has_capacity) validate_capacity(candidate, candidate_capacity)

  if (!is.numeric(transport_cost_rate) || transport_cost_rate < 0)
    stop("`transport_cost_rate` must be a non-negative number.")

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")

  demand <- set_weights(demand, demand_id, demand_weight, "demand")
  weight_col <- if (is.null(demand_weight)) "weight" else demand_weight
  a <- as.numeric(demand[[weight_col]])

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  f_cost <- as.numeric(candidate[[candidate_fixed_cost]])

  if (has_capacity) {
    k_cap <- as.numeric(candidate[[candidate_capacity]])
    if (sum(k_cap) < sum(a))
      stop(sprintf(
        "Total capacity (%.2f) is less than total demand (%.2f) -- no feasible assignment exists.",
        sum(k_cap), sum(a)
      ))
  }

  cost_mat <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                           matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                           cutoff_distance)
  cost_mat <- replace_inf(cost_mat[ids_demand, ids_cand, drop = FALSE])

  model <- ompr::MIPModel() |>
    ompr::add_variable(X[j], j = 1:n_fac, type = "binary") |>
    ompr::add_variable(Y[i, j], i = 1:n_cli, j = 1:n_fac, type = "continuous", lb = 0, ub = 1) |>
    ompr::set_objective(
      ompr::sum_expr(f_cost[j] * X[j], j = 1:n_fac) +
        transport_cost_rate * ompr::sum_expr(a[i] * cost_mat[i, j] * Y[i, j],
                                             i = 1:n_cli, j = 1:n_fac),
      sense = "min"
    ) |>
    ompr::add_constraint(ompr::sum_expr(Y[i, j], j = 1:n_fac) == 1, i = 1:n_cli) |>
    ompr::add_constraint(Y[i, j] <= X[j], i = 1:n_cli, j = 1:n_fac)

  if (has_capacity)
    model <- ompr::add_constraint(
      model, ompr::sum_expr(a[i] * Y[i, j], i = 1:n_cli) <= k_cap[j] * X[j], j = 1:n_fac
    )

  message(sprintf("%s | %d demand points | %d candidates | solver: %s",
                  toupper(model_type), n_cli, n_fac, solver))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf("Solver '%s' failed: %s", solver, e$message))
  )
  if (result$status != "success")
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- ompr::get_solution(result, X[j])$value
  Y_vals <- ompr::get_solution(result, Y[i, j])
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]

  assignments <- extract_assignment(Y_vals, ids_demand, ids_cand, cost_mat)
  fixed_cost_total <- sum(f_cost[selected_j])
  transport_cost_total <- transport_cost_rate *
    sum(a * ifelse(is.finite(assignments$distance), assignments$distance, 0), na.rm = TRUE)

  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]

  build_result(
    model_type = model_type, solver_status = result$status, sf_selected = sf_selected,
    assignments = assignments, fixed_cost_total = fixed_cost_total,
    transport_cost_total = transport_cost_total,
    total_cost = fixed_cost_total + transport_cost_total,
    n_open = length(ids_selected), n_demand = n_cli
  )
}

#' Uncapacitated Fixed-charge Facility Location Problem (UFCLP)
#'
#' Chooses which candidate sites to open, trading off each site's fixed
#' opening cost against the transport cost of serving demand from it.
#' Unlike [p_median()]/[mclp()], the number of open facilities is not
#' fixed -- it falls out of the cost tradeoff.
#'
#' @param demand sf POINT. Demand points.
#' @param demand_id character. Unique id column in `demand`.
#' @param demand_weight character or NULL. Weight column in `demand`.
#' @param candidate sf POINT. Candidate facility sites.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param candidate_weight character or NULL. Unused.
#' @param matrix_OD_candidates data.frame. Long OD table demand-to-candidate.
#' @param matrix_OD_candidates_from_id character.
#' @param matrix_OD_candidates_to_id character.
#' @param matrix_OD_candidates_dist character.
#' @param cutoff_distance numeric. Pairs beyond this distance are dropped.
#' @param candidate_fixed_cost character. Column in `candidate` holding the
#'   fixed cost of opening each site (f_j).
#' @param transport_cost_rate numeric. Cost per unit distance per unit
#'   demand (alpha). Default 1.
#' @param solver character. ROI solver, default `"glpk"`.
#' @return An object of class `llocalocal_result`.
#' @export
ufclp <- function(demand, demand_id, demand_weight = NULL,
                   candidate, candidate_id, candidate_weight = NULL,
                   matrix_OD_candidates,
                   matrix_OD_candidates_from_id = "from_id",
                   matrix_OD_candidates_to_id = "to_id",
                   matrix_OD_candidates_dist = "distance",
                   cutoff_distance = 1000,
                   candidate_fixed_cost,
                   transport_cost_rate = 1,
                   solver = "glpk") {
  .fixed_charge_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id, candidate_weight,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    cutoff_distance, candidate_fixed_cost, candidate_capacity = NULL,
    transport_cost_rate = transport_cost_rate, solver = solver,
    model_type = "ufclp"
  )
}
