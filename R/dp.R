#' Dispersion Problem (DP)
#'
#' Selects `p_facilities` candidate sites to maximize the minimum pairwise
#' distance between opened sites (maximally spread-out placement). Takes
#' no demand layer -- it's a pure site-dispersion problem among
#' `candidate`s.
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
#' @param solver character. ROI solver, default `"glpk"`.
#' @return An object of class `llocalocal_result`.
#' @export
dp <- function(candidate, candidate_id,
                matrix_OD_candidates,
                matrix_OD_candidates_from_id = "from_id",
                matrix_OD_candidates_to_id = "to_id",
                matrix_OD_candidates_dist = "distance",
                p_facilities,
                solver = "glpk") {

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
                           cutoff = Inf)
  dist_mat <- dist_mat[ids_cand, ids_cand, drop = FALSE]
  diag(dist_mat) <- 0
  dist_mat <- replace_inf(dist_mat)
  big_m <- max(dist_mat) * 2

  model <- ompr::MIPModel() |>
    ompr::add_variable(X[j], j = 1:n_fac, type = "binary") |>
    ompr::add_variable(D, type = "continuous", lb = 0) |>
    ompr::set_objective(D, sense = "max") |>
    ompr::add_constraint(ompr::sum_expr(X[j], j = 1:n_fac) == p_facilities) |>
    ompr::add_constraint(
      D <= dist_mat[i, j] + (big_m - dist_mat[i, j]) * (1 - X[i]) +
        (big_m - dist_mat[i, j]) * (1 - X[j]),
      i = 1:n_fac, j = 1:n_fac, i < j
    )

  message(sprintf("DP | %d candidates | p = %d | solver: %s", n_fac, p_facilities, solver))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf("Solver '%s' failed: %s", solver, e$message))
  )
  if (result$status != "success")
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  X_vals <- ompr::get_solution(result, X[j])$value
  D_val <- as.numeric(ompr::get_solution(result, D))
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_cand[selected_j]
  sf_selected <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]

  build_result(
    model_type = "dp", solver_status = result$status, sf_selected = sf_selected,
    min_distance = D_val, n_open = length(ids_selected)
  )
}
