#' Maximum Capture Problem with Price (PMAXCAP)
#'
#' @description
#' Extends [maxcap()] by making the firm's price a decision variable. For
#' a fixed set of open sites, profit is piecewise-linear in price (capture
#' only flips at threshold breakpoints), so the true global optimum is
#' found by enumerating every breakpoint price, solving the resulting
#' MAXCAP-style linear MIP at each one, and keeping the best. This needs
#' no nonlinear/MINLP solver.
#'
#' \deqn{\text{Maximize } \Pi = (P^A - v) \sum_{i \in I} a_i Y_i^A - \sum_{j \in J} f_j X_j^A}
#' \deqn{\text{s.t. } Y_i^A \leq \sum_{j \in N_i(b_i^B)} X_j^A, \; \forall i \qquad \sum_{j=1}^{m} X_j^A = n^A}
#' \deqn{X_j^A, Y_i^A \in \{0,1\}}
#' where \eqn{N_i(b_i^B) = \{j \in J : P^A + t\,d_{ij} < P^B + t\,d_{i,b_i^B}\}}
#' (capture zone, depends on price \eqn{P^A}), \eqn{P^A} = optimized
#' price (returned in `optimal_price`), \eqn{v} = `marginal_cost`,
#' \eqn{f_j} = `candidate_fixed_cost`, \eqn{n^A} = `n_facilities`,
#' \eqn{P^B} = `competitor_price`, \eqn{t} = `distance_cost_rate`. Solved
#' by enumerating price breakpoints (see above), not directly
#' as a MINLP.
#'
#' @inheritParams maxcap
#' @param marginal_cost numeric. Marginal production cost per unit (v).
#' @param distance_cost_rate numeric. Cost per unit distance (t).
#' @param competitor_price numeric. The competitor's fixed price (P_B).
#' @param n_facilities integer. Exact number of new facilities to open (n_A).
#' @param candidate_fixed_cost character or NULL. Column in `candidate` for
#'   the fixed cost of opening each site (f_j, eq. 2.36). Defaults to 0 if
#'   NULL.
#' @param max_breakpoints integer. Caps the number of price breakpoints
#'   evaluated. Under the cap: exact. Over it: breakpoints are subsampled
#'   and a warning is raised -- the result is then an approximation, not
#'   the exact global optimum. Default 2000.
#' @return An object of class `localalloc_result`, with `optimal_price` and
#'   `profit` fields in addition to the usual ones.
#' @export
pmaxcap <- function(demand, demand_id, demand_weight = NULL,
                     candidate, candidate_id,
                     existing_sites, existing_sites_id,
                     matrix_OD_candidates,
                     matrix_OD_candidates_from_id = "from_id",
                     matrix_OD_candidates_to_id = "to_id",
                     matrix_OD_candidates_dist = "distance",
                     matrix_OD_existing_site,
                     matrix_OD_existing_site_from_id = "from_id",
                     matrix_OD_existing_site_to_id = "to_id",
                     matrix_OD_existing_site_dist = "distance",
                     cutoff = NULL,
                     marginal_cost = 0,
                     distance_cost_rate = 1,
                     competitor_price,
                     n_facilities,
                     candidate_fixed_cost = NULL,
                     max_breakpoints = 2000,
                     solver = "highs") {
  t0 <- Sys.time()

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)
  validate_sf(existing_sites, "existing_sites", existing_sites_id)

  # As in [maxcap()], a hard error: `existing_sites` are the competitor's, so an
  # id cannot belong to both layers.
  collision <- intersect(as.character(candidate[[candidate_id]]),
                         as.character(existing_sites[[existing_sites_id]]))
  if (length(collision) > 0)
    stop(sprintf("Ids shared between `candidate` and `existing_sites`: %s",
                 paste(collision, collapse = ", ")))

  if (!is.numeric(n_facilities) || n_facilities < 1)
    stop("`n_facilities` must be an integer >= 1.")
  n_facilities <- as.integer(n_facilities)
  # `max_breakpoints` bounds how many MIPs the price sweep below will solve --
  # it is the model's runtime dial, not a modelling parameter.
  if (!is.numeric(max_breakpoints) || max_breakpoints < 1)
    stop("`max_breakpoints` must be an integer >= 1.")

  if (is.null(cutoff)) {
    cutoff <- Inf
  } else if (!is.numeric(cutoff) || cutoff <= 0) {
    stop("`cutoff` must be NULL (no cutoff) or a positive number.")
  }

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")
  validate_cost_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                       matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                       name = "matrix_OD_existing_site")

  demand <- set_weights(demand, demand_id, demand_weight, "demand")
  weight_col <- if (is.null(demand_weight)) "weight" else demand_weight

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  ids_exist  <- as.character(existing_sites[[existing_sites_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)

  if (n_facilities > n_fac)
    stop(sprintf("`n_facilities` (%d) cannot exceed the number of candidates (%d).",
                 n_facilities, n_fac))

  # f_j is optional here (unlike ufclp/cflp, where it is the point of the model):
  # with no column supplied the fixed-cost term drops out of the profit objective
  # and only the margin on captured demand matters.
  if (!is.null(candidate_fixed_cost)) {
    validate_fixed_cost(candidate, candidate_fixed_cost)
    f_cost <- as.numeric(candidate[[candidate_fixed_cost]])
  } else {
    f_cost <- rep(0, n_fac)
  }

  a <- as.numeric(demand[[weight_col]])

  # Both matrices get a large finite sentinel in place of `Inf`, for the same
  # reason as in [maxcap()]: the model compares delivered costs rather than
  # forbidding pairs, so every cell must be a number the comparisons can read.
  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                cutoff, ids_from = ids_demand, ids_to = ids_cand)
  cost_mat_cand <- replace_inf(cost_mat_cand)

  cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                 matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                 cutoff, ids_from = ids_demand, ids_to = ids_exist)
  cost_mat_exist <- replace_inf(cost_mat_exist)

  # baseline_i = distance from demand i to its nearest competitor site. Combined
  # with `competitor_price` it fixes what the incumbent charges point i all-in:
  # P_B + t * baseline_i, the number our own delivered price has to undercut.
  baseline <- derive_competitor_baseline(cost_mat_exist)

  # ---- The price sweep -----------------------------------------------------
  # Price enters the capture condition, so the set of demand points a site can
  # win *changes with the price we choose* -- a product of two decision
  # variables, which no linear program can express. The way out is that profit is
  # piecewise-constant in price: between two consecutive "someone switches"
  # thresholds, the capture sets are frozen and profit only tracks the margin.
  # The optimum therefore sits exactly on one of those thresholds, and
  # `enumerate_breakpoints()` returns all of them (descending). Fixing the price
  # makes the rest a plain MAXCAP-style MIP, so this loop solves one MIP per
  # candidate price and keeps the best -- an exact global optimum with no
  # nonlinear solver, at the cost of length(breakpoints) solves.
  breakpoints <- enumerate_breakpoints(cost_mat_cand, baseline, distance_cost_rate,
                                       competitor_price, max_breakpoints)

  # `profit = -Inf` so the first feasible solve always wins the comparison.
  best <- list(profit = -Inf, price = NA_real_, X_vals = NULL, Y_vals = NULL, status = NA_character_)
  # ---- Variable layout: [Y, X] (identical at every price) -------------------
  # Y_i (columns 1..n_cli)             = 1 if demand point i is captured.
  # X_j (columns n_cli+1..n_cli+n_fac) = 1 if candidate j is opened; candidates
  #   only, the competitor's sites being no site of ours to open.
  #
  # Everything that does not depend on the price is built once, outside the loop:
  # the budget row, the variable types and the bounds. Only the objective (which
  # scales with the margin) and the capture rows (which depend on who switches at
  # that price) are rebuilt per iteration.
  n_vars <- n_cli + n_fac
  # ---- Budget row: sum_j X_j == n_A ----------------------------------------
  # `==`, unlike [maxcap()]'s `<=`: with fixed costs in the objective an extra
  # site can *lower* profit, so the count is an exact requirement of the
  # formulation rather than a cap the optimum happens to reach.
  A_p <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = n_cli + seq_len(n_fac), x = 1,
                              dims = c(1, n_vars))
  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  # ---- Short-circuit for degenerate breakpoints (price <= marginal_cost) ----
  # Once the price no longer exceeds the marginal cost, every term
  # a_i*(price - marginal_cost) of the profit is <= 0 (assuming a_i >= 0,
  # which demand weights -- population/counts -- always satisfy in
  # practice): capturing any demand can only match or reduce profit, so the
  # optimal solution captures nothing (Y = 0), whichever subset of
  # n_facilities candidates gets opened -- only the fixed cost breaks the
  # tie. That is a trivial computation (a sort), not a MIP.
  #
  # Leaving these breakpoints to the solver is both pointless and costly:
  # the coverage matrix is very dense there (at low prices, nearly every
  # candidate satisfies the capture threshold for nearly every demand
  # point), which makes the MIP pathologically degenerate -- any subset of
  # candidates reaches the same optimum. GLPK already slows down noticeably
  # on these instances; HiGHS, whose presolve does not reliably interrupt
  # itself despite a `time_limit`, can get stuck there for tens of seconds
  # per breakpoint. Short-circuiting this case fixes the problem at the
  # root, for both solvers, without touching solve_direct() or the other
  # models that share it.
  #
  # `breakpoints` is sorted in descending order (see enumerate_breakpoints()),
  # so as soon as one breakpoint falls below `marginal_cost`, every
  # subsequent one (smaller) satisfies the same condition: this case only
  # needs handling once, after which the loop below can ignore the rest.
  trivial_idx <- which(breakpoints <= marginal_cost)[1]
  if (!is.na(trivial_idx)) {
    order_f <- order(f_cost)[seq_len(n_facilities)]
    trivial_profit <- -sum(f_cost[order_f])
    if (trivial_profit > best$profit) {
      X_vals_trivial <- rep(0, n_fac); X_vals_trivial[order_f] <- 1
      best <- list(profit = trivial_profit, price = breakpoints[trivial_idx],
                   X_vals = X_vals_trivial, Y_vals = rep(0, n_cli), status = "optimal")
    }
    breakpoints <- breakpoints[seq_len(trivial_idx - 1)]
  }

  # Reuses the previous breakpoint's solution as the starting point for the
  # next one (HiGHS only -- see highs_start in solve_direct()): consecutive
  # breakpoints only differ by a few columns of bij, so the previous solution
  # is already very close to the next optimum. Verified empirically: the
  # hardest part of the ramp (low prices) drops from several seconds to a
  # few milliseconds per breakpoint.
  prev_solution <- NULL

  for (price in breakpoints) {
    # b_ij at this price: demand i prefers our site j when our delivered cost is
    # no higher than the incumbent's, price + t*d_ij <= P_B + t*baseline_i.
    # Rearranged, `threshold` is the highest price at which j still wins i, so the
    # test is simply `threshold >= price`.
    #
    # Note `>=`: at a price exactly on the breakpoint the point is ours. That is
    # the opposite tie-break from [maxcap()]'s strict `<`, and it is deliberate --
    # the breakpoints *are* the prices being tested, so excluding equality would
    # discard the very cases the sweep enumerates.
    bij <- matrix(0L, nrow = n_cli, ncol = n_fac, dimnames = list(ids_demand, ids_cand))
    for (jj in seq_len(n_fac)) {
      threshold <- competitor_price + distance_cost_rate * baseline -
        distance_cost_rate * cost_mat_cand[, jj]
      bij[, jj] <- as.integer(threshold >= price)
    }

    cov <- which(bij == 1, arr.ind = TRUE)
    # ---- Objective: profit at this price ----------------------------------
    # (P - v) on each a_i Y_i (unit margin times quantity captured) minus f_j on
    # each X_j. The price is a constant inside this iteration, which is exactly
    # what makes the objective linear.
    L <- c(a * (price - marginal_cost), -f_cost)
    # ---- Capture rows: Y_i - sum_j b_ij X_j <= 0, one per demand point ----
    # Same shape as in [maxcap()]: a point may only be claimed if some open site
    # actually wins it at this price.
    A_cov <- Matrix::sparseMatrix(
      i = c(seq_len(n_cli), cov[, 1]), j = c(seq_len(n_cli), n_cli + cov[, 2]),
      x = c(rep(1, n_cli), rep(-1, nrow(cov))), dims = c(n_cli, n_vars))
    A <- rbind(A_p, A_cov)
    dir <- c("==", rep("<=", n_cli))
    rhs <- c(n_facilities, rep(0, n_cli))

    # `presolve = "off"` (only when solver = "highs"): specific to
    # pmaxcap(). At low prices the coverage matrix bij becomes very dense
    # (many candidates satisfy the capture threshold for many demand
    # points), and HiGHS's presolve becomes pathologically slow there --
    # verified empirically: a breakpoint that took 5-17s drops to 0.6-1.7s.
    # Not passed to solve_direct() by default (NULL) because
    # lscp()/maxcap()/pcenter() don't share this characteristic and instead
    # benefit fully from HiGHS's presolve.
    result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver,
                            highs_control = if (solver == "highs") list(presolve = "off") else NULL,
                            highs_start = prev_solution)
    # Non-optimal iterations are skipped rather than raising: one bad price does
    # not invalidate the sweep, and the `is.null(best$X_vals)` check below catches
    # the case where *every* price failed. `>` (not `>=`) keeps the first winner,
    # and since breakpoints are in descending order that is the highest price
    # among equally profitable ones.
    if (result$optimal) {
      profit <- result$objective_value
      if (profit > best$profit) {
        best <- list(profit = profit, price = price,
                     X_vals = result$solution[(n_cli + 1):(n_cli + n_fac)],
                     Y_vals = result$solution[seq_len(n_cli)],
                     status = result$status)
      }
      # Starting point for the next breakpoint, whether or not this
      # iteration beat `best`: it's the closest available solution to the
      # next problem, regardless of whether it was the best one overall.
      prev_solution <- result$solution
    }
  }

  if (is.null(best$X_vals))
    stop("No feasible solution found across any price breakpoint.")

  message(sprintf("PMAXCAP | %d demand points | %d candidates | n = %d | %d breakpoints | solver: %s",
                  n_cli, n_fac, n_facilities, length(breakpoints), solver))

  # ---- Decode the winning iteration ----------------------------------------
  # `best` holds the [Y, X] blocks of the single most profitable price, already
  # sliced apart inside the loop. `round()` because a solver reports an integral
  # variable as 0.9999999 rather than exactly 1.
  selected_j <- which(round(best$X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  # Candidates only: the `existing_sites` belong to the competitor, so
  # `bind_selected_sites()` is deliberately not used.
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  # Quantity captured at the optimal price. Note this is *not* the objective
  # value -- `profit` is, and it also nets off the margin and the fixed costs.
  covered_demand <- sum(a[round(best$Y_vals) == 1])

  build_result(
    model_type = "pmaxcap", solver_status = best$status, sf_selected = sf_selected,
    covered_demand = covered_demand, optimal_price = best$price, profit = best$profit,
    n_open = length(ids_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
