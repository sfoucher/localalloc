#' Dispersion Problem (DP)
#'
#' @description
#' Selects `p_facilities` candidate sites to maximize the minimum pairwise
#' distance between opened sites (maximally spread-out placement). Takes
#' no demand layer -- it's a pure site-dispersion problem among
#' `candidate`s.
#'
#' \deqn{\text{Maximize } D}
#' \deqn{\text{s.t. } D \leq d_{ij} + (M - d_{ij})(1-X_i) + (M-d_{ij})(1-X_j), \; \forall i,j}
#' \deqn{\sum_{j} X_j = P \qquad X_j \in \{0,1\}}
#' where \eqn{D} = `min_distance` in the output, \eqn{P} = `p_facilities`,
#' and \eqn{M} is a sufficiently large constant (computed internally as
#' 2 times the maximum distance in the matrix).
#'
#' @param candidate sf POINT. Candidate sites.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param matrix_OD_candidates data.frame. Long candidate-to-candidate
#'   distance table (from_id/to_id/distance) -- both directions of each
#'   pair should be supplied.
#' @param matrix_OD_candidates_from_id character.
#' @param matrix_OD_candidates_to_id character.
#' @param matrix_OD_candidates_dist character.
#' @param p_facilities integer. Number of sites to select (>= 2).
#' @param solver character. `"highs"` (default) or `"glpk"`.
#' @return An object of class `localocal_result`.
#' @export
dp <- function(candidate, candidate_id,
                matrix_OD_candidates,
                matrix_OD_candidates_from_id = "from_id",
                matrix_OD_candidates_to_id = "to_id",
                matrix_OD_candidates_dist = "distance",
                p_facilities,
                solver = "highs") {
  t0 <- Sys.time()

  validate_sf(candidate, "candidate", candidate_id)
  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")

  # Two sites minimum: with p = 1 there is no pair to measure, so "the minimum
  # pairwise distance" is undefined and every candidate would tie.
  if (!is.numeric(p_facilities) || p_facilities < 2)
    stop("`p_facilities` must be an integer >= 2 (dispersion needs at least 2 sites).")
  p_facilities <- as.integer(p_facilities)

  ids_cand <- as.character(candidate[[candidate_id]])
  n_fac <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

  # Square candidate-to-candidate matrix: unlike every other model here, the OD
  # table's `from` and `to` are both the candidate layer -- there is no demand.
  # No cutoff is applied (`cutoff = Inf`): dropping far-apart pairs would remove
  # exactly the pairs dispersion is looking for.
  dist_mat <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                           matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                           cutoff = Inf, ids_from = ids_cand, ids_to = ids_cand)
  # A site is at distance 0 from itself. The OD table usually omits those rows,
  # leaving `Inf` on the diagonal; the diagonal is never read below (only
  # upper-triangle pairs are), but zeroing it keeps `max(dist_mat)` honest.
  diag(dist_mat) <- 0
  # Any pair still `Inf` is missing from the OD table. There is no "forbidden
  # pair" concept in DP -- every pair needs a constraint row -- so missing pairs
  # are given a large finite distance. Caveat: that reads as "maximally far
  # apart", which makes such pairs *attractive* to a dispersion objective. Supply
  # the complete pairwise matrix if the result is to be trusted.
  dist_mat <- replace_inf(dist_mat)
  # Big-M must exceed any value D could take, i.e. the largest pairwise distance;
  # 2x that is comfortably above it while staying in the same order of magnitude
  # (an unnecessarily huge M weakens the LP relaxation and slows the solve).
  big_m <- max(dist_mat) * 2

  message("DP | building sparse MIP...")
  # ---- Variable layout: [X, D] ---------------------------------------------
  # X_j (columns 1..n_fac)   = 1 if candidate j is opened.
  # D   (the last column, `d_col`) = continuous, the smallest distance between
  #   any two *open* sites. As in p_center's Z, "the minimum over a set the model
  #   is still choosing" cannot be written as a linear objective, so it becomes a
  #   variable: the constraints press it down, the objective pulls it up, and at
  #   the optimum it equals the true minimum.
  #
  # Only the upper triangle: the dispersion constraint is symmetric in (i, j), so
  # each unordered pair needs one row, not two. Note this is one row per pair --
  # the model is quadratic in the number of candidates, and that (not the solve
  # itself) is what limits DP to modest candidate sets.
  pairs <- which(upper.tri(matrix(TRUE, n_fac, n_fac)), arr.ind = TRUE)
  idx_i <- pairs[, 1]; idx_j <- pairs[, 2]
  n_pairs <- nrow(pairs)
  d_col <- n_fac + 1
  n_vars <- d_col

  # D <= dist_ij + (M-dist_ij)*(1-X_i) + (M-dist_ij)*(1-X_j) expands to
  # D + c_ij*X_i + c_ij*X_j <= 2M - dist_ij, where c_ij = M - dist_ij.
  #
  # Read the three cases of the original form:
  #   both sites open  (X_i = X_j = 1) -> D <= dist_ij, the row bites;
  #   one site open                    -> D <= M, non-binding by construction;
  #   neither open                     -> D <= 2M - dist_ij, non-binding.
  # So only pairs of *open* sites constrain D, which is precisely "the minimum
  # distance among the selected sites" without ever naming which pair it is.
  dist_ij <- dist_mat[cbind(idx_i, idx_j)]
  c_ij <- big_m - dist_ij

  # ---- Objective: maximize D -----------------------------------------------
  # D alone carries a coefficient; opening a site is free (the count is pinned by
  # the budget row). The solver therefore spreads the p sites as far apart as it
  # can, judged by their *closest* pair -- a pure minimax-style criterion with no
  # demand anywhere in it.
  L <- c(rep(0, n_fac), 1)
  # ---- Budget row: sum_j X_j == p ------------------------------------------
  # Exactly p sites, not "at most": with `<=` the model would open the 2 sites
  # furthest apart and stop, since every extra site can only lower D.
  A_p <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = seq_len(n_fac), x = 1, dims = c(1, n_vars))
  # ---- Dispersion rows: one per candidate pair -----------------------------
  # Three non-zeros per row -- `+1` on D and `c_ij` on each of the pair's two X
  # columns -- which is the rearranged big-M form derived above. `rep(1:n_pairs, 3)`
  # repeats the row index for those three coefficient groups.
  A_disp <- Matrix::sparseMatrix(
    i = rep(seq_len(n_pairs), 3), j = c(rep(d_col, n_pairs), idx_i, idx_j),
    x = c(rep(1, n_pairs), c_ij, c_ij), dims = c(n_pairs, n_vars))
  A <- rbind(A_p, A_disp)
  dir <- c("==", rep("<=", n_pairs))
  rhs <- c(p_facilities, 2 * big_m - dist_ij)

  # X binary; D continuous ("C") and unbounded above -- its ceiling comes from
  # the dispersion rows, not from a bound.
  types <- c(rep("B", n_fac), "C")
  lower <- c(rep(0, n_fac), 0)
  upper <- c(rep(1, n_fac), Inf)

  message(sprintf("DP | solving | %d candidates | p = %d | solver: %s", n_fac, p_facilities, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  # ---- Decode the solution -------------------------------------------------
  # Slice the flat solution vector back into the [X, D] blocks. Position j in the
  # X block is candidate j in `ids_cand`; `round()` because a solver reports an
  # integral variable as 0.9999999 rather than exactly 1.
  X_vals <- result$solution[seq_len(n_fac)]
  # D at the optimum is the maximized closest-pair distance -- the model's
  # headline result, reported as `min_distance`.
  D_val <- result$solution[d_col]
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]

  build_result(
    model_type = "dp", solver_status = result$status, sf_selected = sf_selected,
    min_distance = D_val, n_open = length(ids_selected),
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
