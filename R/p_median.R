# Shared engine for the two assignment-style p-facility models that differ only
# in the direction of the objective: `p_median()` (sense = "min", the classic
# problem) and `uflp()` (sense = "max", the deliberately worst-case variant used
# as a benchmark). Everything else -- variable layout, constraints, decoding --
# is identical, which is why they share this body.
#
# "Assignment-style" means the model does not merely decide *which* sites open;
# it also pairs every demand point with the one facility that serves it, so the
# objective can price the actual distance travelled. That is the difference from
# LSCP/MCLP, which only ever ask whether a pair is within a radius.
.assignment_model <- function(demand, demand_id, demand_weight,
                               candidate, candidate_id,
                               existing_sites, existing_sites_id,
                               matrix_OD_candidates, matrix_OD_candidates_from_id,
                               matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                               matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                               matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                               cutoff, p_facilities, solver,
                               sense, model_type) {
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

  if (is.null(cutoff)) {
    cutoff <- Inf
  } else if (!is.numeric(cutoff) || cutoff <= 0) {
    stop("`cutoff` must be NULL (no cutoff) or a positive number.")
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
  weight_demand <- as.numeric(demand[[weight_col]])

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

  message(sprintf("%s | building cost matrix (%d demand points x %d candidates)...",
                  toupper(model_type), n_cli, n_fac))
  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                cutoff, ids_from = ids_demand, ids_to = ids_cand)

  if (has_existing) {
    ids_exist <- as.character(existing_sites[[existing_sites_id]])
    cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                   matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                   cutoff, ids_from = ids_demand, ids_to = ids_exist)
    ids_all_fac <- c(ids_cand, ids_exist)
    cost_mat <- cbind(cost_mat_cand, cost_mat_exist)
  } else {
    ids_all_fac <- ids_cand
    cost_mat <- cost_mat_cand
  }
  n_all_fac <- length(ids_all_fac)

  # Every demand point must be assigned to exactly one facility, so a point with
  # no reachable site within `cutoff` makes the model infeasible: its assignment
  # row would read `0 == 1`. Stop here naming the offending points rather than
  # letting the solver return an opaque "infeasible".
  # TODO: decide whether this should be a warning (dropping the point) instead.
  uncovered <- ids_demand[!apply(is.finite(cost_mat), 1, any)]
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) have no candidate or existing site within `cutoff` (%.3g): %s",
      length(uncovered), cutoff, paste(uncovered, collapse = ", ")
    ))

  message(sprintf("%s | building sparse MIP...", toupper(model_type)))
  # ---- Variable layout: [Y, X] ---------------------------------------------
  # `valid` is the sparse list of *allowed* (demand, facility) pairs -- the cells
  # of the cost matrix that are finite, i.e. present in the OD table and within
  # `cutoff`. This is the key sparsity decision: a pair that cannot be used gets
  # no variable at all, rather than a variable the solver must reason about.
  #
  # Y_k (columns 1..n_y) = 1 if demand `idx_i[k]` is served by facility
  #   `idx_j[k]`; one variable per row of `valid`, NOT the full n_cli x n_fac
  #   grid. So Y is indexed by pair number k, and the (i, j) it refers to is
  #   recovered through idx_i[k] / idx_j[k].
  # X_j (columns n_y+1..n_y+n_all_fac) = 1 if facility j is opened; candidates
  #   first, then existing sites.
  valid <- which(is.finite(cost_mat), arr.ind = TRUE)
  idx_i <- valid[, 1]; idx_j <- valid[, 2]
  n_y <- nrow(valid)
  n_vars <- n_y + n_all_fac

  # ---- Objective: sum over pairs of a_i * d_ij * Y_ij ----------------------
  # Each Y variable's coefficient is the weight of its demand point times the
  # distance of its pair, so choosing Y_k "charges" that trip. X carries 0 --
  # opening a facility is free here, the count being fixed by the budget row.
  # Minimizing (p_median) therefore drives every point onto its nearest open
  # facility and pushes the open set towards the demand's weighted centres;
  # maximizing (uflp) does the opposite.
  L <- c(weight_demand[idx_i] * cost_mat[cbind(idx_i, idx_j)], rep(0, n_all_fac))

  # ---- Budget row: sum_j X_j == p ------------------------------------------
  # One row of 1s across the whole X block -- candidates *and* existing sites --
  # so p counts every open facility. With the X_j == 1 rows below, k existing
  # sites eat k of the p slots and only p - k candidates can still be opened.
  A_p      <- Matrix::sparseMatrix(i = rep(1L, n_all_fac), j = n_y + seq_len(n_all_fac), x = 1,
                                   dims = c(1, n_vars))
  # ---- Assignment rows: sum_j Y_ij == 1, one per demand point --------------
  # Row i holds a 1 in the column of every pair variable belonging to demand i
  # (`i = idx_i` scatters each pair into its demand's row). Exactly one of them
  # may be 1, so each point is served once and only once.
  A_assign <- Matrix::sparseMatrix(i = idx_i, j = seq_len(n_y), x = 1,
                                   dims = c(n_cli, n_vars))
  # ---- Linking rows: Y_ij - X_j <= 0, one per pair -------------------------
  # `+1` on the pair variable, `-1` on its facility's X. Reading a row: a demand
  # point may only be assigned to a facility that is actually open. Without
  # these the assignment rows would happily route demand to closed sites and the
  # budget row would be meaningless. This is the constraint that couples the two
  # blocks, and it is what makes the selection of X respond to the distances in
  # the objective.
  A_link   <- Matrix::sparseMatrix(i = rep(seq_len(n_y), 2), j = c(seq_len(n_y), n_y + idx_j),
                                   x = c(rep(1, n_y), rep(-1, n_y)), dims = c(n_y, n_vars))
  A <- rbind(A_p, A_assign, A_link)
  dir <- c("==", rep("==", n_cli), rep("<=", n_y))
  rhs <- c(p_facilities, rep(1, n_cli), rep(0, n_y))

  if (has_existing) {
    # ---- Required Facilities: X_j == 1 for each existing site --------------
    # One row per existing site pinning its variable to 1. Because these columns
    # also sit in the budget row, forcing them open is what consumes part of p.
    # Their linking rows still apply, so demand may be assigned to them freely.
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_y + n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  # Both blocks binary: Y assigned / not, X open / not.
  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("%s | solving | %d demand points | %d candidates (p=%d total open)%s | solver: %s",
                  toupper(model_type), n_cli, n_fac, p_facilities,
                  if (has_existing)
                    sprintf(" | %d existing (forced) -> %d candidate(s) to select",
                            n_exist, p_facilities - n_exist)
                  else "",
                  solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = sense, solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  # ---- Decode the solution -------------------------------------------------
  # Slice the flat solution vector back into the [Y, X] blocks. The Y block is
  # rebuilt as (i, j, value) triplets, re-attaching each pair variable to the
  # demand/facility indices it stood for, so `extract_assignment()` can turn it
  # back into one row per demand point.
  Y_vals <- data.frame(i = idx_i, j = idx_j, value = result$solution[seq_len(n_y)])
  X_vals <- result$solution[(n_y + 1):(n_y + n_all_fac)]
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

  # Recompute the objective from the decoded assignments: sum of a_i * d_ij over
  # the chosen pairs. `assignments` is in demand order, matching `weight_demand`.
  # The `ifelse` contributes 0 for any point left unassigned (only reachable on a
  # non-optimal solve, where `distance` is NA).
  total_cost <- sum(weight_demand * ifelse(is.finite(assignments$distance), assignments$distance, 0),
                    na.rm = TRUE)

  sf_selected <- bind_selected_sites(candidate, candidate_id, ids_selected,
                                     if (has_existing) existing_sites else NULL,
                                     existing_sites_id)

  build_result(
    model_type = model_type, solver_status = result$status, sf_selected = sf_selected,
    assignments = assignments, total_cost = total_cost,
    n_open = nrow(sf_selected),
    n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

#' P-Median Problem
#'
#' Opens exactly `p_facilities` facilities in total to minimize the total
#' weighted distance between each demand point and its nearest open
#' facility. Sites in `existing_sites` are forced open from the start
#' ("Required Facilities") and *count against* `p_facilities`: with
#' `k` existing sites, only `p_facilities - k` candidates are selected.
#'
#' @details
#' \deqn{\text{Minimize } z = \sum_{i=1}^{n} \sum_{j=1}^{m} a_i d_{ij} Y_{ij}}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} Y_{ij} = 1, \; \forall i \qquad Y_{ij} \leq X_j, \; \forall i,j \qquad \sum_{j=1}^{m} X_j = p}
#' \deqn{X_j, Y_{ij} \in \{0,1\}}
#' where \eqn{a_i} = `demand_weight`, \eqn{d_{ij}} = distance (OD matrix),
#' \eqn{p} = `p_facilities`, and \eqn{j} indexes candidate *and* existing
#' sites (the latter fixed at \eqn{X_j = 1}).
#'
#' @inheritParams lscp
#' @param demand_weight character or NULL. Weight column in `demand` (e.g.
#'   population). This is the primary driver of the objective -- candidates
#'   are chosen to minimize (p_median) or maximize (uflp) total weighted
#'   assigned distance. Defaults to 1 if NULL.
#' @param p_facilities integer. Total number of open facilities, counting
#'   both the forced-open `existing_sites` and the candidates selected by
#'   the model. Must be >= the number of `existing_sites` and <= the number
#'   of candidates plus existing sites.
#' @return An object of class `localalloc_result`. Its `sf_selected` layer
#'   lists *every* open facility -- the selected candidates and the
#'   forced-open `existing_sites` -- with a `source` column
#'   (`"candidate"` / `"existing"`) telling them apart.
#' @export
p_median <- function(demand, demand_id, demand_weight = NULL,
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
                      cutoff = NULL,
                      p_facilities,
                      solver = "highs") {
  .assignment_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id,
    existing_sites, existing_sites_id,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    matrix_OD_existing_site, matrix_OD_existing_site_from_id,
    matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
    cutoff, p_facilities, solver,
    sense = "min", model_type = "p_median"
  )
}
