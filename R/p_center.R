#' P-Center Problem
#'
#'@description
#' Opens exactly `p_facilities` facilities in total to minimize the maximum
#' distance between any demand point and its assigned facility (minimax).
#' Sites in `existing_sites` are forced open ("Required Facilities") and
#' *count against* `p_facilities`: with `k` existing sites, only
#' `p_facilities - k` candidates are selected.
#'
#' \deqn{\text{Minimize } z}
#' \deqn{\text{s.t. } z \geq d_{ij} Y_{ij}, \; \forall i,j \qquad \sum_{j=1}^{m} Y_{ij} = 1, \; \forall i}
#' \deqn{Y_{ij} \leq X_j, \; \forall i,j \qquad \sum_{j=1}^{m} X_j = p \qquad X_j, Y_{ij} \in \{0,1\}}
#' where \eqn{d_{ij}} = distance (OD matrix), \eqn{p} = `p_facilities`,
#' \eqn{z} is the model's `Z` variable (maximum distance).
#'
#' @inheritParams lscp
#' @param p_facilities integer. Total number of open facilities, counting
#'   both the forced-open `existing_sites` and the candidates selected by
#'   the model. Must be >= the number of `existing_sites` and <= the number
#'   of candidates plus existing sites.
#' @return An object of class `localalloc_result`. Its `sf_selected` layer
#'   lists *every* open facility -- the selected candidates and the
#'   forced-open `existing_sites` -- with a `source` column
#'   (`"candidate"` / `"existing"`) telling them apart.
#' @export
p_center <- function(demand, demand_id,
                      candidate, candidate_id,
                      existing_sites = NULL, existing_sites_id = NULL,
                      matrix_OD_candidates,
                      matrix_OD_candidates_from_id = "from_id",
                      matrix_OD_candidates_to_id = "to_id",
                      matrix_OD_candidates_dist = "distance",
                      matrix_OD_existing_site = NULL,
                      matrix_OD_existing_site_from_id = "from_id",
                      matrix_OD_existing_site_to_id = "to_id",
                      matrix_OD_existing_site_dist = "distance",
                      cutoff_distance = NULL,
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
  if (has_existing)
    validate_cost_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                         matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                         name = "matrix_OD_existing_site")

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  n_exist <- if (has_existing) nrow(existing_sites) else 0L
  # `p_facilities` is the total number of open facilities (existing + selected
  # candidates), so it is bounded below by the forced-open existing sites and
  # above by every site available.
  if (p_facilities > n_fac + n_exist)
    stop(sprintf("`p_facilities` (%d) cannot exceed the total number of available sites (%d candidate(s) + %d existing).",
                 p_facilities, n_fac, n_exist))
  if (p_facilities < n_exist)
    stop(sprintf("`p_facilities` (%d) cannot be lower than the number of `existing_sites` (%d), which are forced open.",
                 p_facilities, n_exist))

  message(sprintf("P_CENTER | building cost matrix (%d demand points x %d candidates)...",
                  n_cli, n_fac))
  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                cutoff_distance, ids_from = ids_demand, ids_to = ids_cand)

  if (has_existing) {
    ids_exist <- as.character(existing_sites[[existing_sites_id]])
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
  cost_mat <- cost_mat_all

  # Every demand point must be assigned to exactly one facility, so a point with
  # no reachable site within `cutoff_distance` makes the model infeasible: its
  # assignment row would read `0 == 1`. Stop here naming the offending points
  # rather than letting the solver return an opaque "infeasible".
  # TODO: decide whether this should be a warning (dropping the point) instead.
  uncovered <- ids_demand[!apply(is.finite(cost_mat), 1, any)]
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) have no candidate or existing site within `cutoff_distance` (%.3g): %s",
      length(uncovered), cutoff_distance, paste(uncovered, collapse = ", ")
    ))

  message("P_CENTER | building sparse MIP...")
  # ---- Variable layout: [Y, X, Z] ------------------------------------------
  # `valid` is the sparse list of *allowed* (demand, facility) pairs -- the
  # finite cells of the cost matrix (present in the OD table, within cutoff).
  # Disallowed pairs get no variable at all, which is what keeps the MIP sparse.
  #
  # Y_k (columns 1..n_y) = 1 if demand `idx_i[k]` is served by facility
  #   `idx_j[k]`; one variable per row of `valid`, so Y is indexed by pair
  #   number k and its (i, j) is read back via idx_i[k] / idx_j[k].
  # X_j (columns n_y+1..n_y+n_all_fac) = 1 if facility j is opened; candidates
  #   first, then existing sites.
  # Z   (the single last column, `z_col`) = continuous, the largest assigned
  #   distance in the solution. Z is the whole trick of the minimax formulation:
  #   "the worst distance" is not something a linear objective can express
  #   directly, so it is introduced as a variable, squeezed from below by the
  #   constraints and from above by the objective.
  valid <- which(is.finite(cost_mat), arr.ind = TRUE)
  idx_i <- valid[, 1]; idx_j <- valid[, 2]
  n_y <- nrow(valid)
  z_col <- n_y + n_all_fac + 1
  n_vars <- z_col

  # ---- Objective: minimize Z -----------------------------------------------
  # Z alone carries a coefficient; Y and X are free. So the model is indifferent
  # to total travel (that is p_median's job) and cares only about the single
  # worst-served demand point.
  L <- c(rep(0, n_y + n_all_fac), 1)

  # ---- Budget row: sum_j X_j == p ------------------------------------------
  # One row of 1s across the whole X block -- candidates *and* existing sites --
  # so p counts every open facility, and with the X_j == 1 rows below, k existing
  # sites consume k of the p slots.
  A_p      <- Matrix::sparseMatrix(i = rep(1L, n_all_fac), j = n_y + seq_len(n_all_fac), x = 1,
                                   dims = c(1, n_vars))
  # ---- Assignment rows: sum_j Y_ij == 1, one per demand point --------------
  # Row i holds a 1 in the column of every pair variable belonging to demand i,
  # so each point is served exactly once.
  A_assign <- Matrix::sparseMatrix(i = idx_i, j = seq_len(n_y), x = 1,
                                   dims = c(n_cli, n_vars))
  # ---- Linking rows: Y_ij - X_j <= 0, one per pair -------------------------
  # `+1` on the pair variable, `-1` on its facility's X: demand may only be
  # assigned to a facility that is actually open. This couples the two blocks.
  A_link   <- Matrix::sparseMatrix(i = rep(seq_len(n_y), 2), j = c(seq_len(n_y), n_y + idx_j),
                                   x = c(rep(1, n_y), rep(-1, n_y)), dims = c(n_y, n_vars))
  # ---- Minimax rows: sum_j d_ij Y_ij - Z <= 0, one per demand point --------
  # Row i carries each of demand i's pair variables weighted by that pair's
  # distance, plus `-1` on Z. Since exactly one Y per row is 1 (assignment rows),
  # the sum collapses to the distance actually travelled by point i, and the row
  # says: Z >= that distance. Stacked over all points, Z is pushed up to at
  # least the maximum assigned distance; the objective pulls it back down, so at
  # the optimum Z *equals* that maximum. Minimizing Z therefore selects the p
  # sites that make the worst-off demand point as well-served as possible.
  A_minimax <- Matrix::sparseMatrix(
    i = c(idx_i, seq_len(n_cli)), j = c(seq_len(n_y), rep(z_col, n_cli)),
    x = c(cost_mat[cbind(idx_i, idx_j)], rep(-1, n_cli)), dims = c(n_cli, n_vars))
  A <- rbind(A_p, A_assign, A_link, A_minimax)
  dir <- c("==", rep("==", n_cli), rep("<=", n_y), rep("<=", n_cli))
  rhs <- c(p_facilities, rep(1, n_cli), rep(0, n_y), rep(0, n_cli))

  if (has_existing) {
    # ---- Required Facilities: X_j == 1 for each existing site --------------
    # One row per existing site pinning its variable to 1. These columns sit in
    # the budget row too, which is what makes existing sites count against p.
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_y + n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  # Y and X binary; Z continuous ("C") and unbounded above -- its ceiling comes
  # from the minimax rows, not from a bound.
  types <- c(rep("B", n_y + n_all_fac), "C")
  lower <- c(rep(0, n_y + n_all_fac), 0)
  upper <- c(rep(1, n_y + n_all_fac), Inf)

  message(sprintf("P_CENTER | solving | %d demand points | %d candidates (p=%d total open)%s | solver: %s",
                  n_cli, n_fac, p_facilities,
                  if (has_existing)
                    sprintf(" | %d existing (forced) -> %d candidate(s) to select",
                            n_exist, p_facilities - n_exist)
                  else "",
                  solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "min", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  # ---- Decode the solution -------------------------------------------------
  # Slice the flat solution vector back into the [Y, X, Z] blocks. The Y block is
  # rebuilt as (i, j, value) triplets, re-attaching each pair variable to the
  # demand/facility indices it stood for.
  Y_vals <- data.frame(i = idx_i, j = idx_j, value = result$solution[seq_len(n_y)])
  X_vals <- result$solution[(n_y + 1):(n_y + n_all_fac)]
  # Z's value at the optimum is the minimized worst-case distance -- the model's
  # headline result, reported as `max_distance`.
  max_distance <- result$solution[z_col]
  # Only the first n_fac entries of X are candidates; the existing sites that
  # follow are re-appended by `bind_selected_sites()` from their own layer.
  # `round()` because a solver reports an integral variable as 0.9999999.
  selected_j <- which(round(X_vals[1:n_fac]) == 1)
  ids_selected <- ids_cand[selected_j]

  assignments <- extract_assignment(Y_vals, ids_demand, ids_all_fac, cost_mat)
  # Tag each served point by the kind of facility serving it. Ids are unique
  # across the two layers (collisions were removed during validation), so
  # membership in `ids_cand` is a reliable test.
  assignments$source <- ifelse(assignments$facility_id %in% ids_cand, "candidate", "existing")

  sf_selected <- bind_selected_sites(candidate, candidate_id, ids_selected,
                                     if (has_existing) existing_sites else NULL,
                                     existing_sites_id)

  build_result(
    model_type = "p_center", solver_status = result$status, sf_selected = sf_selected,
    assignments = assignments, max_distance = max_distance,
    n_open = nrow(sf_selected),
    n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
