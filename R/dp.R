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
#' @details
#' # Asymmetric matrices: the row order of `candidate` affects the answer
#'
#' The dispersion constraint is symmetric in \eqn{(i, j)}, so the model builds
#' one row per *unordered* pair and reads only the upper triangle of the
#' distance matrix. When a pair disagrees between its two directions --
#' `A -> B = 10` but `B -> A = 15`, routine with travel times because of
#' one-way streets and slope -- the two values are not combined, averaged or
#' reconciled. **One of them is silently discarded**, and which one depends on
#' the order the sites appear in `candidate`: the value kept is the one whose
#' `from_id` comes first.
#'
#' This is not flagged by any check, because an asymmetric matrix is legitimate
#' data. But it does mean an incidental reordering upstream (a
#' `candidate[order(candidate$id), ]`, a different `dplyr` pipeline) can change
#' both `min_distance` and the sites returned, on identical input:
#'
#' ```
#' # candidate order A,B,C  ->  sites B,C  |  min_distance = 5750
#' # candidate order A,C,B  ->  sites A,C  |  min_distance = 4950
#' ```
#'
#' If your matrix is asymmetric, symmetrize it yourself before calling `dp()`
#' so the choice is explicit rather than an artefact of row order. Keeping the
#' **smaller** of the two directions is the conservative option: `min_distance`
#' then reads as a guarantee -- no two selected sites are closer than \eqn{D} --
#' that holds whichever way each pair is travelled.
#'
#' @param candidate sf POINT. Candidate sites. Its row order is not neutral
#'   when the distance matrix is asymmetric; see Details.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param matrix_OD_candidates data.frame. Long candidate-to-candidate
#'   distance table (from_id/to_id/distance) -- both directions of each
#'   pair should be supplied. Pairs missing from the upper triangle are
#'   warned about and given a large finite distance; see Details for how
#'   directions that disagree are resolved.
#' @param matrix_OD_candidates_from_id character.
#' @param matrix_OD_candidates_to_id character.
#' @param matrix_OD_candidates_dist character.
#' @param p_facilities integer. Number of sites to select (>= 2).
#' @param solver character. `"highs"` (default) or `"glpk"`.
#' @return An object of class `localalloc_result`.
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

  # An OD table holding no candidate-to-candidate pair leaves `dist_mat` entirely
  # `Inf`, and that fails *silently* rather than loudly: the diagonal is zeroed
  # just below, `replace_inf()` then derives its big-M as `max(finite) * 10` --
  # which is now `0` -- so every pair reads as distance 0, the dispersion rows
  # collapse to `D <= 0`, and the solve happily reports `min_distance = 0` for an
  # arbitrary set of p sites. The usual cause is passing the demand-to-candidate
  # OD table the other nine models take: its `from_id`s are demand ids and match
  # no candidate, so nothing is ever scattered into the matrix. Report the id
  # overlap in both columns, since that is what pins down which table was passed.
  #
  # `finite_cells` is computed once and reused by the coverage warning below;
  # comparing counts rather than subsetting an off-diagonal mask keeps this to a
  # single n x n allocation, and DP's candidate sets are already the largest
  # thing in memory here.
  finite_cells <- is.finite(dist_mat)
  if (sum(finite_cells) <= sum(diag(finite_cells))) {
    n_from <- sum(ids_cand %in% as.character(matrix_OD_candidates[[matrix_OD_candidates_from_id]]))
    n_to   <- sum(ids_cand %in% as.character(matrix_OD_candidates[[matrix_OD_candidates_to_id]]))
    stop(sprintf(paste0(
      "`matrix_OD_candidates` contains no candidate-to-candidate pair ",
      "(%d of %d candidate id(s) appear in '%s', %d in '%s'). `dp()` needs a ",
      "pairwise distance table *between candidates* -- both '%s' and '%s' must ",
      "be drawn from `candidate` -- not a demand-to-candidate table."),
      n_from, n_fac, matrix_OD_candidates_from_id,
      n_to, matrix_OD_candidates_to_id,
      matrix_OD_candidates_from_id, matrix_OD_candidates_to_id))
  }

  # Some pairs present, but not all. Only the upper triangle is read further
  # down, and every cell of it still `Inf` is about to be handed a fabricated
  # distance by `replace_inf()` -- `max * 10`, i.e. "further apart than any real
  # pair". For a dispersion objective that is the worst possible direction to err
  # in: missing data does not merely blur the answer, it makes the unknown pairs
  # look ideal and biases the selection towards the very sites nothing is known
  # about. Warn rather than fail, since a deliberately partial matrix is a
  # legitimate choice once the caller knows this is the consequence.
  #
  # `reverse_only` is broken out because it has a different and much cheaper fix:
  # those pairs *are* in the OD table, just under the opposite (to, from)
  # orientation, so symmetrizing the table recovers them with no new routing.
  # This is the likeliest cause of a partial matrix, since routers commonly
  # return one row per ordered pair and `dp()` needs both.
  upper <- upper.tri(finite_cells)
  upper_finite <- finite_cells[upper]
  n_possible <- n_fac * (n_fac - 1) / 2
  n_missing <- n_possible - sum(upper_finite)
  if (n_missing > 0) {
    # t(finite_cells)[i, j] is finite_cells[j, i]: the same pair measured the
    # other way round.
    reverse_only <- sum(!upper_finite & t(finite_cells)[upper])
    msg <- sprintf(paste0(
      "%d of %d candidate pair(s) (%.1f%%) are missing from ",
      "`matrix_OD_candidates`. Missing pairs are given a large finite distance, ",
      "which reads as 'maximally far apart' and therefore makes them attractive ",
      "to the dispersion objective -- `min_distance` and the selected sites may ",
      "be driven by absent data rather than by real distances."),
      n_missing, n_possible, 100 * n_missing / n_possible)
    if (reverse_only > 0)
      msg <- paste0(msg, sprintf(
        " %d of them are present in the opposite direction only; supply both directions of each pair.",
        reverse_only))
    warning(msg, call. = FALSE)
  }

  # TODO: handle pairs whose two directions *disagree*. Nothing below reconciles
  # `dist_mat[i, j]` with `dist_mat[j, i]`: only the upper triangle is read, so
  # the value kept is whichever site comes first in `candidate` and the other is
  # silently dropped. Row order therefore changes both `min_distance` and the
  # selected sites on identical input (documented under Details). With travel
  # times rather than metres this is the normal case, not an edge case -- one-way
  # streets and slope make d(A,B) != d(B,A) routine.
  #
  # Two options, deliberately left out for now because they change results on
  # existing calls:
  #   * symmetrize to the minimum -- `pmin(dist_mat, t(dist_mat))` right here,
  #     which makes `min_distance` a guarantee that holds in both travel
  #     directions. Costs one more n x n allocation.
  #   * or just warn past a threshold, e.g. when the relative gap
  #     `|d_ij - d_ji| / pmin(d_ij, d_ji)` exceeds some tolerance on any pair,
  #     leaving the choice to the caller.
  # A `symmetrize = c("none", "min")` argument defaulting to "none" would keep
  # this backward compatible. Whichever lands, it belongs above `replace_inf()`,
  # since the fabricated big-M values below would otherwise be compared against
  # real distances.

  # A site is at distance 0 from itself. The OD table usually omits those rows,
  # leaving `Inf` on the diagonal; the diagonal is never read below (only
  # upper-triangle pairs are), but zeroing it keeps `max(dist_mat)` honest.
  diag(dist_mat) <- 0
  # Any pair still `Inf` is missing from the OD table. There is no "forbidden
  # pair" concept in DP -- every pair needs a constraint row -- so missing pairs
  # are given a large finite distance here. That is the substitution the checks
  # above exist to police: it reads as "maximally far apart", which makes such
  # pairs *attractive* to a dispersion objective. The caller has been warned with
  # a count by this point; supply the complete pairwise matrix if the result is
  # to be trusted.
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
