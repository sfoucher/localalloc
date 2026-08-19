#' Maximum Capture Problem with Price (PMAXCAP)
#'
#' Extends [maxcap()] by making the firm's price a decision variable. For
#' a fixed set of open sites, profit is piecewise-linear in price (capture
#' only flips at threshold breakpoints), so the true global optimum is
#' found by enumerating every breakpoint price, solving the resulting
#' MAXCAP-style linear MIP at each one, and keeping the best. This needs
#' no nonlinear/MINLP solver.
#'
#' @details
#' \deqn{\text{Maximize } \Pi = (P^A - v) \sum_{i \in I} a_i Y_i^A - \sum_{j \in J} f_j X_j^A}
#' \deqn{\text{s.t. } Y_i^A \leq \sum_{j \in N_i(b_i^B)} X_j^A, \; \forall i \qquad \sum_{j=1}^{m} X_j^A = n^A}
#' \deqn{X_j^A, Y_i^A \in \{0,1\}}
#' where \eqn{N_i(b_i^B) = \{j \in J : P^A + t\,d_{ij} < P^B + t\,d_{i,b_i^B}\}}
#' (capture zone, depends on price \eqn{P^A}), \eqn{P^A} = optimized
#' price (returned in `optimal_price`), \eqn{v} = `marginal_cost`,
#' \eqn{f_j} = `candidate_fixed_cost`, \eqn{n^A} = `n_facilities`,
#' \eqn{P^B} = `competitor_price`, \eqn{t} = `distance_cost_rate`. Solved
#' by enumerating price breakpoints (see description above), not directly
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
#' @return An object of class `llocalocal_result`, with `optimal_price` and
#'   `profit` fields in addition to the usual ones.
#' @export
pmaxcap <- function(demand, demand_id, demand_weight = NULL,
                     candidate, candidate_id,
                     existing_sites, existing_sites_id,
                     existing_sites_weight = NULL,
                     matrix_OD_candidates,
                     matrix_OD_candidates_from_id = "from_id",
                     matrix_OD_candidates_to_id = "to_id",
                     matrix_OD_candidates_dist = "distance",
                     matrix_OD_existing_site,
                     matrix_OD_existing_site_from_id = "from_id",
                     matrix_OD_existing_site_to_id = "to_id",
                     matrix_OD_existing_site_dist = "distance",
                     cutoff_distance = NULL,
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

  collision <- intersect(as.character(candidate[[candidate_id]]),
                         as.character(existing_sites[[existing_sites_id]]))
  if (length(collision) > 0)
    stop(sprintf("Ids shared between `candidate` and `existing_sites`: %s",
                 paste(collision, collapse = ", ")))

  if (!is.numeric(n_facilities) || n_facilities < 1)
    stop("`n_facilities` must be an integer >= 1.")
  n_facilities <- as.integer(n_facilities)
  if (!is.numeric(max_breakpoints) || max_breakpoints < 1)
    stop("`max_breakpoints` must be an integer >= 1.")

  if (is.null(cutoff_distance)) {
    cutoff_distance <- Inf
  } else if (!is.numeric(cutoff_distance) || cutoff_distance <= 0) {
    stop("`cutoff_distance` must be NULL (no cutoff) or a positive number.")
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

  if (!is.null(candidate_fixed_cost)) {
    validate_fixed_cost(candidate, candidate_fixed_cost)
    f_cost <- as.numeric(candidate[[candidate_fixed_cost]])
  } else {
    f_cost <- rep(0, n_fac)
  }

  a <- as.numeric(demand[[weight_col]])

  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                cutoff_distance, ids_from = ids_demand, ids_to = ids_cand)
  cost_mat_cand <- replace_inf(cost_mat_cand)

  cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                 matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                 cutoff_distance, ids_from = ids_demand, ids_to = ids_exist)
  cost_mat_exist <- replace_inf(cost_mat_exist)

  baseline <- derive_competitor_baseline(cost_mat_exist)

  breakpoints <- enumerate_breakpoints(cost_mat_cand, baseline, distance_cost_rate,
                                       competitor_price, max_breakpoints)

  best <- list(profit = -Inf, price = NA_real_, X_vals = NULL, Y_vals = NULL, status = NA_character_)
  n_vars <- n_cli + n_fac
  A_p <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = n_cli + seq_len(n_fac), x = 1,
                              dims = c(1, n_vars))
  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  for (price in breakpoints) {
    bij <- matrix(0L, nrow = n_cli, ncol = n_fac, dimnames = list(ids_demand, ids_cand))
    for (jj in seq_len(n_fac)) {
      threshold <- competitor_price + distance_cost_rate * baseline -
        distance_cost_rate * cost_mat_cand[, jj]
      bij[, jj] <- as.integer(threshold >= price)
    }

    cov <- which(bij == 1, arr.ind = TRUE)
    L <- c(a * (price - marginal_cost), -f_cost)
    A_cov <- Matrix::sparseMatrix(
      i = c(seq_len(n_cli), cov[, 1]), j = c(seq_len(n_cli), n_cli + cov[, 2]),
      x = c(rep(1, n_cli), rep(-1, nrow(cov))), dims = c(n_cli, n_vars))
    A <- rbind(A_p, A_cov)
    dir <- c("==", rep("<=", n_cli))
    rhs <- c(n_facilities, rep(0, n_cli))

    result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver)
    if (result$optimal) {
      profit <- result$objective_value
      if (profit > best$profit) {
        best <- list(profit = profit, price = price,
                     X_vals = result$solution[(n_cli + 1):(n_cli + n_fac)],
                     Y_vals = result$solution[seq_len(n_cli)],
                     status = result$status)
      }
    }
  }

  if (is.null(best$X_vals))
    stop("No feasible solution found across any price breakpoint.")

  message(sprintf("PMAXCAP | %d demand points | %d candidates | n = %d | %d breakpoints | solver: %s",
                  n_cli, n_fac, n_facilities, length(breakpoints), solver))

  selected_j <- which(round(best$X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  covered_demand <- sum(a[round(best$Y_vals) == 1])

  build_result(
    model_type = "pmaxcap", solver_status = best$status, sf_selected = sf_selected,
    covered_demand = covered_demand, optimal_price = best$price, profit = best$profit,
    n_open = length(ids_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
