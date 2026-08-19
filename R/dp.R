#' Dispersion Problem (DP)
#'
#' Selects `p_facilities` candidate sites to maximize the minimum pairwise
#' distance between opened sites (maximally spread-out placement). Takes
#' no demand layer -- it's a pure site-dispersion problem among
#' `candidate`s.
#'
#' @details
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
#' @return An object of class `llocalocal_result`.
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

  if (!is.numeric(p_facilities) || p_facilities < 2)
    stop("`p_facilities` must be an integer >= 2 (dispersion needs at least 2 sites).")
  p_facilities <- as.integer(p_facilities)

  ids_cand <- as.character(candidate[[candidate_id]])
  n_fac <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf("`p_facilities` (%d) cannot exceed the number of candidates (%d).",
                 p_facilities, n_fac))

  dist_mat <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                           matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                           cutoff = Inf, ids_from = ids_cand, ids_to = ids_cand)
  diag(dist_mat) <- 0
  dist_mat <- replace_inf(dist_mat)
  big_m <- max(dist_mat) * 2

  message("DP | building sparse MIP...")
  pairs <- which(upper.tri(matrix(TRUE, n_fac, n_fac)), arr.ind = TRUE)
  idx_i <- pairs[, 1]; idx_j <- pairs[, 2]
  n_pairs <- nrow(pairs)
  d_col <- n_fac + 1
  n_vars <- d_col

  # D <= dist_ij + (M-dist_ij)*(1-X_i) + (M-dist_ij)*(1-X_j) expands to
  # D + c_ij*X_i + c_ij*X_j <= 2M - dist_ij, where c_ij = M - dist_ij.
  dist_ij <- dist_mat[cbind(idx_i, idx_j)]
  c_ij <- big_m - dist_ij

  L <- c(rep(0, n_fac), 1)
  A_p <- Matrix::sparseMatrix(i = rep(1L, n_fac), j = seq_len(n_fac), x = 1, dims = c(1, n_vars))
  A_disp <- Matrix::sparseMatrix(
    i = rep(seq_len(n_pairs), 3), j = c(rep(d_col, n_pairs), idx_i, idx_j),
    x = c(rep(1, n_pairs), c_ij, c_ij), dims = c(n_pairs, n_vars))
  A <- rbind(A_p, A_disp)
  dir <- c("==", rep("<=", n_pairs))
  rhs <- c(p_facilities, 2 * big_m - dist_ij)

  types <- c(rep("B", n_fac), "C")
  lower <- c(rep(0, n_fac), 0)
  upper <- c(rep(1, n_fac), Inf)

  message(sprintf("DP | solving | %d candidates | p = %d | solver: %s", n_fac, p_facilities, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "max", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- result$solution[seq_len(n_fac)]
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
