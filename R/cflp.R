#' Capacitated Fixed-charge Facility Location Problem (CFLP)
#'
#' Extends [ufclp()] with a capacity limit per facility: the total
#' weighted demand assigned to a site cannot exceed its capacity.
#'
#' @inheritParams ufclp
#' @param candidate_capacity character. Column in `candidate` holding each
#'   site's maximum capacity (k_j).
#' @return An object of class `llocalocal_result`.
#' @export
cflp <- function(demand, demand_id, demand_weight = NULL,
                  candidate, candidate_id, candidate_weight = NULL,
                  matrix_OD_candidates,
                  matrix_OD_candidates_from_id = "from_id",
                  matrix_OD_candidates_to_id = "to_id",
                  matrix_OD_candidates_dist = "distance",
                  cutoff_distance = 1000,
                  candidate_fixed_cost,
                  candidate_capacity,
                  transport_cost_rate = 1,
                  solver = "glpk") {
  .fixed_charge_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id, candidate_weight,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    cutoff_distance, candidate_fixed_cost, candidate_capacity,
    transport_cost_rate = transport_cost_rate, solver = solver,
    model_type = "cflp"
  )
}
