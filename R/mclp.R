#' Maximal Covering Location Problem (MCLP)
#'
#'@description
#' Opens `p_facilities` facilities in total to maximize the weighted
#' demand covered within `service_radius`. Unlike [lscp()], full coverage
#' of every demand point is not required -- demand that can't be reached
#' by any facility within the radius is simply left uncovered, which is
#' the point of "maximal" coverage rather than total coverage.
#' `existing_sites`, if supplied, are forced open ("Required Facilities",
#' same semantics as [p_median()]/[p_center()]) and *count against*
#' `p_facilities`: with `k` existing sites, only `p_facilities - k`
#' candidates are selected.
#'
#' \deqn{\text{Maximize } z = \sum_{i=1}^{n} a_i Y_i}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} b_{ij} X_j \geq Y_i, \; \forall i \qquad \sum_{j=1}^{m} X_j = p}
#' \deqn{X_j, Y_i \in \{0,1\}}
#' where \eqn{a_i} = `demand_weight`, \eqn{b_{ij} = 1} if \eqn{d_{ij} \leq S}
#' (`service_radius`), \eqn{p} = `p_facilities`.
#'
#' @inheritParams lscp
#' @param demand_weight character or NULL. Weight column in `demand` (e.g.
#'   population). This is the primary driver of MCLP's objective --
#'   candidates are chosen to maximize total weighted covered demand.
#'   Defaults to 1 if NULL.
#' @param service_radius numeric. Maximum acceptable distance.
#' @param p_facilities integer. Total number of open facilities, counting
#'   both the forced-open `existing_sites` and the candidates selected by
#'   the model. Must be >= the number of `existing_sites` and <= the number
#'   of candidates plus existing sites.
#' @return An object of class `localalloc_result`. Its `sf_selected` layer
#'   lists *every* open facility -- the selected candidates and the
#'   forced-open `existing_sites` -- with a `source` column
#'   (`"candidate"` / `"existing"`) telling them apart.
#' @export
mclp <- function(demand, demand_id, demand_weight = NULL,
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
                  service_radius,
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

  if (!is.numeric(service_radius) || length(service_radius) != 1 || service_radius <= 0)
    stop("`service_radius` must be a single positive number.")
  if (is.null(cutoff_distance)) {
    cutoff_distance <- Inf
  } else if (!is.numeric(cutoff_distance) || cutoff_distance <= 0) {
    stop("`cutoff_distance` must be NULL (no cutoff) or a positive number.")
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
  a <- as.numeric(demand[[weight_col]])

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
  # b_ij = 1 when facility j covers demand i (d_ij <= service_radius), else 0.
  # Unlike LSCP there is no feasibility check here: a demand point covered by
  # nothing is legal in MCLP, it simply stays uncovered (Y_i forced to 0).
  bij <- make_coverage_matrix(cost_mat_all, service_radius)

  message("MCLP | building sparse MIP...")
  # ---- Variable layout: [Y, X] ---------------------------------------------
  # Y_i (columns 1..n_cli)              = 1 if demand point i is covered.
  # X_j (columns n_cli+1..n_cli+n_all_fac) = 1 if facility j is opened;
  #                                          candidates first, then existing.
  # Y here is *coverage*, not assignment -- MCLP never says which facility
  # serves a point, only whether some open one covers it.
  #
  # `cov` lists only the covering (i, j) pairs, so only those become non-zeros
  # in the coverage rows.
  cov <- which(bij == 1, arr.ind = TRUE)
  n_vars <- n_cli + n_all_fac

  # ---- Objective: maximize covered weighted demand -------------------------
  # Coefficient a_i on each Y_i, 0 on every X_j (opening a facility carries no
  # cost in MCLP -- the count is capped by the budget row instead). The solver
  # thus picks the p sites whose combined coverage carries the most weight.
  L <- c(a, rep(0, n_all_fac))

  # ---- Budget row: sum_j X_j == p ------------------------------------------
  # A single row of 1s across the whole X block -- candidates *and* existing
  # sites -- so p is the total number of open facilities, not the number of new
  # ones. Combined with the X_j == 1 rows below, k existing sites consume k of
  # the p slots and only p - k candidates can still be opened.
  A_p    <- Matrix::sparseMatrix(i = rep(1L, n_all_fac), j = n_cli + seq_len(n_all_fac), x = 1,
                                 dims = c(1, n_vars))
  # ---- Coverage rows: Y_i - sum_j b_ij X_j <= 0, one per demand point ------
  # Written as `+1` on Y_i and `-1` on each covering X_j, which is the
  # rearranged form of Y_i <= sum_j b_ij X_j. It only forbids claiming coverage
  # that isn't there; it never *forces* Y_i to 1. The objective does that, since
  # every Y_i it can legally raise to 1 adds a_i to the total.
  A_cov  <- Matrix::sparseMatrix(
    i = c(seq_len(n_cli), cov[, 1]), j = c(seq_len(n_cli), n_cli + cov[, 2]),
    x = c(rep(1, n_cli), rep(-1, nrow(cov))), dims = c(n_cli, n_vars))
  A <- rbind(A_p, A_cov)
  dir <- c("==", rep("<=", n_cli))
  rhs <- c(p_facilities, rep(0, n_cli))

  if (has_existing) {
    # ---- Required Facilities: X_j == 1 for each existing site --------------
    # One row per existing site pinning its variable to 1. Note these rows share
    # the budget row above, which is exactly why existing sites count against p.
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_cli + n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  # Both blocks binary: Y_i covered / not, X_j open / not.
  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("MCLP | solving | %d demand points | %d candidates | radius = %g | p = %d total open%s | solver: %s",
                  n_cli, n_fac, service_radius, p_facilities,
                  if (has_existing)
                    sprintf(" | %d existing (forced) -> %d candidate(s) to select",
                            n_exist, p_facilities - n_exist)
                  else "",
                  solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  # ---- Decode the solution -------------------------------------------------
  # Slice the flat solution vector back into the [Y, X] blocks of the layout.
  Y_vals <- result$solution[seq_len(n_cli)]
  X_vals <- result$solution[(n_cli + 1):(n_cli + n_all_fac)]
  # Only the first n_fac entries of X are candidates; the rest are the existing
  # sites, which `bind_selected_sites()` re-appends from their own layer. `round()`
  # because a solver reports an integral variable as 0.9999999 rather than 1.
  selected_j <- which(round(X_vals[1:n_fac]) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- bind_selected_sites(candidate, candidate_id, ids_selected,
                                     if (has_existing) existing_sites else NULL,
                                     existing_sites_id)
  # Sum the weights of the demand points flagged as covered -- this equals the
  # objective value, recomputed from Y for readability.
  covered_demand <- sum(a[round(Y_vals) == 1])

  build_result(
    model_type = "mclp", solver_status = result$status, sf_selected = sf_selected,
    covered_demand = covered_demand,
    n_open = nrow(sf_selected),
    n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
