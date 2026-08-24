# Shared engine for the two fixed-charge models, which differ only by the
# presence of a capacity limit: `ufclp()` (uncapacitated, `candidate_capacity =
# NULL`) and `cflp()` (capacitated). The capacity rows are the only block of the
# MIP that is conditional; everything else -- variable layout, objective,
# assignment/linking rows, decoding -- is identical, which is why both wrappers
# call this body.
#
# What sets these two apart from every other model in the package: the number of
# open facilities is *not* an input. There is no budget row, no `p_facilities`.
# The count falls out of a cost tradeoff -- each site charges a fixed cost f_j
# to open, and every unit of demand charges transport cost. Opening more sites
# shortens trips but adds fixed cost; the optimum is wherever the two balance.
#
# `model_type` is passed in only to label the messages and the result object;
# the mathematics is selected by `candidate_capacity` being NULL or not.
.fixed_charge_model <- function(demand, demand_id, demand_weight,
                                 candidate, candidate_id,
                                 matrix_OD_candidates, matrix_OD_candidates_from_id,
                                 matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                 cutoff_distance, candidate_fixed_cost, candidate_capacity,
                                 transport_cost_rate, solver, model_type) {
  t0 <- Sys.time()

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)
  validate_fixed_cost(candidate, candidate_fixed_cost)
  # This single flag is the whole difference between ufclp() and cflp(): it
  # switches the capacity rows on and off further down.
  has_capacity <- !is.null(candidate_capacity)
  if (has_capacity) validate_capacity(candidate, candidate_capacity)

  if (!is.numeric(transport_cost_rate) || transport_cost_rate < 0)
    stop("`transport_cost_rate` must be a non-negative number.")

  if (is.null(cutoff_distance)) {
    cutoff_distance <- Inf
  } else if (!is.numeric(cutoff_distance) || cutoff_distance <= 0) {
    stop("`cutoff_distance` must be NULL (no cutoff) or a positive number.")
  }

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")

  # `a_i` is the demand quantity of point i. It plays two roles here: it scales
  # the transport term of the objective, and (when capacities are on) it is the
  # quantity consuming a facility's capacity. `set_weights()` fills a column of
  # 1s when the caller supplied none, so `a` is always defined.
  demand <- set_weights(demand, demand_id, demand_weight, "demand")
  weight_col <- if (is.null(demand_weight)) "weight" else demand_weight
  a <- as.numeric(demand[[weight_col]])

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand); n_fac <- length(ids_cand)
  # f_j: the cost of opening candidate j, read off the candidate layer. Unlike
  # the p-facility models, no site is free -- this vector is the objective's
  # entire "open a facility" term.
  f_cost <- as.numeric(candidate[[candidate_fixed_cost]])

  if (has_capacity) {
    k_cap <- as.numeric(candidate[[candidate_capacity]])
    # Cheap global feasibility test: even with every site open, the capacity
    # rows cannot absorb more demand than sum(k_j). Catch that here with a
    # message naming both totals, rather than after a full solve reporting only
    # "infeasible". Note this is necessary but not sufficient -- capacity can
    # still be unreachable per-point once `cutoff_distance` is applied.
    if (sum(k_cap) < sum(a))
      stop(sprintf(
        "Total capacity (%.2f) is less than total demand (%.2f) -- no feasible assignment exists.",
        sum(k_cap), sum(a)
      ))
  }

  message(sprintf("%s | building cost matrix (%d demand points x %d candidates)...",
                  toupper(model_type), n_cli, n_fac))
  cost_mat <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                           matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                           cutoff_distance, ids_from = ids_demand, ids_to = ids_cand)

  # Every demand point must be assigned to exactly one facility, so a point with
  # no reachable candidate within `cutoff_distance` makes the model infeasible:
  # its assignment row would read `0 == 1`. Fail here naming the offending points
  # rather than letting the solver return an opaque "infeasible".
  uncovered <- ids_demand[!apply(is.finite(cost_mat), 1, any)]
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) have no candidate within `cutoff_distance` (%.3g): %s",
      length(uncovered), cutoff_distance, paste(uncovered, collapse = ", ")
    ))

  message(sprintf("%s | building sparse MIP...", toupper(model_type)))
  # ---- Variable layout: [Y, X] ---------------------------------------------
  # `valid` is the sparse list of *allowed* (demand, facility) pairs -- the
  # finite cells of the cost matrix (present in the OD table, within cutoff).
  # A pair that cannot be used gets no variable at all, which is what keeps the
  # MIP sparse instead of n_cli x n_fac dense.
  #
  # Y_k (columns 1..n_y) = share of demand `idx_i[k]` served by facility
  #   `idx_j[k]`; one variable per row of `valid`, so Y is indexed by pair
  #   number k and its (i, j) is read back through idx_i[k] / idx_j[k].
  # X_j (columns n_y+1..n_y+n_fac) = 1 if candidate j is opened. There is no
  #   existing-sites block in these two models: `existing_sites` is not even a
  #   parameter of ufclp()/cflp().
  valid <- which(is.finite(cost_mat), arr.ind = TRUE)
  idx_i <- valid[, 1]; idx_j <- valid[, 2]
  n_y <- nrow(valid)
  n_vars <- n_y + n_fac

  # ---- Objective: fixed costs + transport costs ----------------------------
  # Y's coefficient is alpha * a_i * d_ij: serving point i from j costs its
  # demand quantity times the distance, priced at `transport_cost_rate`.
  # X's coefficient is f_j flat. Minimizing the sum is the tradeoff that decides
  # how many sites open -- an extra site is worth its f_j only if it shortens
  # enough trips to pay for itself. Setting `transport_cost_rate = 0` collapses
  # the model to "open nothing but what feasibility demands"; a large rate drives
  # it towards opening everything.
  L <- c(transport_cost_rate * a[idx_i] * cost_mat[cbind(idx_i, idx_j)], f_cost)

  # ---- Assignment rows: sum_j Y_ij == 1, one per demand point --------------
  # Row i holds a 1 in the column of every pair variable belonging to demand i
  # (`i = idx_i` scatters each pair into its demand's row), so each point's
  # shares must add up to exactly its whole demand -- nothing unserved, nothing
  # served twice.
  A_assign <- Matrix::sparseMatrix(i = idx_i, j = seq_len(n_y), x = 1, dims = c(n_cli, n_vars))
  # ---- Linking rows: Y_ij - X_j <= 0, one per pair -------------------------
  # `+1` on the pair variable, `-1` on its facility's X. Reading a row: demand
  # may only be routed to a facility that is actually open. This is the only
  # thing forcing the model to pay any f_j at all -- without it the assignment
  # rows would be satisfied for free with every X_j at 0.
  A_link   <- Matrix::sparseMatrix(i = rep(seq_len(n_y), 2), j = c(seq_len(n_y), n_y + idx_j),
                                   x = c(rep(1, n_y), rep(-1, n_y)), dims = c(n_y, n_vars))
  A <- rbind(A_assign, A_link)
  dir <- c(rep("==", n_cli), rep("<=", n_y))
  rhs <- c(rep(1, n_cli), rep(0, n_y))

  if (has_capacity) {
    # ---- Capacity rows (CFLP only): sum_i a_i Y_ij - k_j X_j <= 0 ----------
    # One row per candidate, so `i = idx_j` groups the pair variables by their
    # *facility* -- the transpose of how the assignment rows group them. Each
    # pair contributes a_i (the demand quantity it would move), and the facility's
    # own column carries -k_j, giving `served_j <= k_j * X_j`.
    #
    # Note the row does double duty: with X_j = 0 the right side is 0, so it also
    # forbids serving from a closed site. `A_link` is kept regardless because it
    # is a much tighter LP relaxation (per-pair rather than per-facility), which
    # is what keeps the branch-and-bound tree small.
    A_cap <- Matrix::sparseMatrix(
      i = c(idx_j, seq_len(n_fac)), j = c(seq_len(n_y), n_y + seq_len(n_fac)),
      x = c(a[idx_i], -k_cap), dims = c(n_fac, n_vars))
    A <- rbind(A, A_cap)
    dir <- c(dir, rep("<=", n_fac))
    rhs <- c(rhs, rep(0, n_fac))
  }

  # X binary (open / not open), but Y *continuous* on [0, 1] -- the one place in
  # the package where an assignment variable is not binary.
  #
  # For UFCLP this is free: with no capacity, the cheapest way to satisfy an
  # assignment row is to put the whole share on the nearest open facility, so an
  # integral optimum always exists (only exact distance ties admit a fractional
  # one, at the same objective value) and leaving Y continuous saves the solver
  # from branching on n_y variables.
  # For CFLP it is a modelling choice: a capacity limit can make splitting
  # genuinely cheaper, so demand is treated as divisible. If a point's demand
  # does get split, `extract_assignment()` reports it as unassigned (no single
  # Y_ij rounds to 1) -- use integer Y if single-sourcing is required.
  types <- c(rep("C", n_y), rep("B", n_fac))
  lower <- c(rep(0, n_y), rep(0, n_fac))
  upper <- c(rep(1, n_y), rep(1, n_fac))

  message(sprintf("%s | solving | %d demand points | %d candidates | solver: %s",
                  toupper(model_type), n_cli, n_fac, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "min", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  # ---- Decode the solution -------------------------------------------------
  # Slice the flat solution vector back into the [Y, X] blocks. The Y block is
  # rebuilt as (i, j, value) triplets, re-attaching each pair variable to the
  # demand/facility indices it stood for, so `extract_assignment()` can turn it
  # back into one row per demand point. `round()` because a solver reports an
  # integral variable as 0.9999999 rather than exactly 1.
  X_vals <- result$solution[(n_y + 1):(n_y + n_fac)]
  Y_vals <- data.frame(i = idx_i, j = idx_j, value = result$solution[seq_len(n_y)])
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]

  assignments <- extract_assignment(Y_vals, ids_demand, ids_cand, cost_mat)
  # Report the objective split into its two halves rather than just the total,
  # since the whole point of the model is the balance between them. Both are
  # recomputed from the decoded solution instead of read off
  # `result$objective_value`, so they stay meaningful even on a non-optimal
  # solve, and `total_cost` below is their sum by construction.
  fixed_cost_total <- sum(f_cost[selected_j])
  # `assignments` is in demand order, matching `a`. The `ifelse` contributes 0
  # for any point reported unassigned -- a non-optimal solve, or (CFLP) demand
  # split across facilities, in which case this total is an underestimate.
  transport_cost_total <- transport_cost_rate *
    sum(a * ifelse(is.finite(assignments$distance), assignments$distance, 0), na.rm = TRUE)

  # Candidates only: these models have no `existing_sites`, so there is nothing
  # for `bind_selected_sites()` to reconcile and no `source` column.
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]

  build_result(
    model_type = model_type, solver_status = result$status, sf_selected = sf_selected,
    assignments = assignments, fixed_cost_total = fixed_cost_total,
    transport_cost_total = transport_cost_total,
    total_cost = fixed_cost_total + transport_cost_total,
    n_open = length(ids_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

#' Uncapacitated Fixed-charge Facility Location Problem (UFCLP)
#'
#'@description
#' Chooses which candidate sites to open, trading off each site's fixed
#' opening cost against the transport cost of serving demand from it.
#' Unlike [p_median()]/[mclp()], the number of open facilities is not
#' fixed -- it falls out of the cost tradeoff.
#'
#' \deqn{\text{Minimize } z = \sum_{j=1}^{m} f_j X_j + \alpha \sum_{i=1}^{n} \sum_{j=1}^{m} a_i d_{ij} Y_{ij}}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} Y_{ij} = 1, \; \forall i \qquad Y_{ij} \leq X_j, \; \forall i,j}
#' \deqn{X_j \in \{0,1\} \qquad Y_{ij} \geq 0}
#' where \eqn{f_j} = `candidate_fixed_cost`, \eqn{\alpha} = `transport_cost_rate`,
#' \eqn{a_i} = `demand_weight`, \eqn{d_{ij}} = distance (OD matrix).
#'
#' @param demand sf POINT. Demand points.
#' @param demand_id character. Unique id column in `demand`.
#' @param demand_weight character or NULL. Weight column in `demand`.
#' @param candidate sf POINT. Candidate facility sites.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param matrix_OD_candidates data.frame. Long OD table demand-to-candidate.
#' @param matrix_OD_candidates_from_id character.
#' @param matrix_OD_candidates_to_id character.
#' @param matrix_OD_candidates_dist character.
#' @param cutoff_distance numeric or NULL. Pairs beyond this distance are
#'   dropped. `NULL` (default) means no cutoff.
#' @param candidate_fixed_cost character. Column in `candidate` holding the
#'   fixed cost of opening each site (f_j).
#' @param transport_cost_rate numeric. Cost per unit distance per unit
#'   demand (alpha). Default 1.
#' @param solver character. `"highs"` (default) or `"glpk"`.
#' @return An object of class `localalloc_result`.
#' @export
ufclp <- function(demand, demand_id, demand_weight = NULL,
                   candidate, candidate_id,
                   matrix_OD_candidates,
                   matrix_OD_candidates_from_id = "from_id",
                   matrix_OD_candidates_to_id = "to_id",
                   matrix_OD_candidates_dist = "distance",
                   cutoff_distance = NULL,
                   candidate_fixed_cost,
                   transport_cost_rate = 1,
                   solver = "highs") {
  # Thin wrapper: `candidate_capacity = NULL` is what makes this the
  # *un*capacitated variant -- it switches off the capacity rows in the shared
  # engine at the top of this file. [cflp()] is the same call with a capacity
  # column supplied.
  .fixed_charge_model(
    demand, demand_id, demand_weight,
    candidate, candidate_id,
    matrix_OD_candidates, matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
    cutoff_distance, candidate_fixed_cost, candidate_capacity = NULL,
    transport_cost_rate = transport_cost_rate, solver = solver,
    model_type = "ufclp"
  )
}
