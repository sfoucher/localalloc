#' Undesirable Facility Location Problem (UFLP)
#'
#' The "reverse" of [p_median()]: opens exactly `p_facilities` facilities
#' in total to *maximize* the total weighted distance between each demand
#' point and its assigned facility, pushing undesirable installations as
#' far as possible from demand. As in [p_median()], `existing_sites` are
#' forced open and count against `p_facilities`.
#'
#' @details
#' Same constraints as [p_median()], objective inverted:
#' \deqn{\text{Maximize } z = \sum_{i=1}^{n} \sum_{j=1}^{m} a_i d_{ij} Y_{ij}}
#' where \eqn{a_i} = `demand_weight`, \eqn{d_{ij}} = distance (OD matrix).
#'
#' @inheritParams p_median
#' @return An object of class `localalloc_result`. Its `sf_selected` layer
#'   lists *every* open facility -- the selected candidates and the
#'   forced-open `existing_sites` -- with a `source` column
#'   (`"candidate"` / `"existing"`) telling them apart.
#' @export
uflp <- function(demand, demand_id, demand_weight = NULL,
                  candidate, candidate_id,
                  existing_sites = NULL, existing_sites_id = NULL,
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
  # Thin wrapper: everything happens in `.assignment_model()` (defined in
  # `p_median.R`), which is shared with [p_median()]. `sense = "max"` is the
  # *only* difference between the two -- same variables, same constraints, same
  # decoding; the solver is simply asked to push demand as far from its facility
  # as possible instead of as near. Hence `total_cost` in the result is a
  # distance to be maximized here, not a cost to be minimized.
  .assignment_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id,
    existing_sites, existing_sites_id,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    matrix_OD_existing_site, matrix_OD_existing_site_from_id,
    matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
    cutoff_distance, p_facilities, solver,
    sense = "max", model_type = "uflp"
  )
}
