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

  message(sprintf("%s | building cost matrix (%d demand points x %d candidates)...",
                  toupper(model_type), n_cli, n_fac))
  cost_mat <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                           matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                           cutoff_distance, ids_from = ids_demand, ids_to = ids_cand)

  uncovered <- ids_demand[!apply(is.finite(cost_mat), 1, any)]
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) have no candidate within `cutoff_distance` (%.3g): %s",
      length(uncovered), cutoff_distance, paste(uncovered, collapse = ", ")
    ))

  message(sprintf("%s | building sparse MIP...", toupper(model_type)))
  valid <- which(is.finite(cost_mat), arr.ind = TRUE)
  idx_i <- valid[, 1]; idx_j <- valid[, 2]
  n_y <- nrow(valid)
  n_vars <- n_y + n_fac

  L <- c(transport_cost_rate * a[idx_i] * cost_mat[cbind(idx_i, idx_j)], f_cost)

  A_assign <- Matrix::sparseMatrix(i = idx_i, j = seq_len(n_y), x = 1, dims = c(n_cli, n_vars))
  A_link   <- Matrix::sparseMatrix(i = rep(seq_len(n_y), 2), j = c(seq_len(n_y), n_y + idx_j),
                                   x = c(rep(1, n_y), rep(-1, n_y)), dims = c(n_y, n_vars))
  A <- rbind(A_assign, A_link)
  dir <- c(rep("==", n_cli), rep("<=", n_y))
  rhs <- c(rep(1, n_cli), rep(0, n_y))

  if (has_capacity) {
    A_cap <- Matrix::sparseMatrix(
      i = c(idx_j, seq_len(n_fac)), j = c(seq_len(n_y), n_y + seq_len(n_fac)),
      x = c(a[idx_i], -k_cap), dims = c(n_fac, n_vars))
    A <- rbind(A, A_cap)
    dir <- c(dir, rep("<=", n_fac))
    rhs <- c(rhs, rep(0, n_fac))
  }

  types <- c(rep("C", n_y), rep("B", n_fac))
  lower <- c(rep(0, n_y), rep(0, n_fac))
  upper <- c(rep(1, n_y), rep(1, n_fac))

  message(sprintf("%s | solving | %d demand points | %d candidates | solver: %s",
                  toupper(model_type), n_cli, n_fac, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "min", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- result$solution[(n_y + 1):(n_y + n_fac)]
  Y_vals <- data.frame(i = idx_i, j = idx_j, value = result$solution[seq_len(n_y)])
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
    n_open = length(ids_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

#' Uncapacitated Fixed-charge Facility Location Problem (UFCLP)
#'
#' Chooses which candidate sites to open, trading off each site's fixed
#' opening cost against the transport cost of serving demand from it.
#' Unlike [p_median()]/[mclp()], the number of open facilities is not
#' fixed -- it falls out of the cost tradeoff.
#'
#' @details
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
#' @return An object of class `llocalocal_result`.
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
