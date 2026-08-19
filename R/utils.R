#' @importFrom sf st_geometry
#' @noRd
NULL

#' Solve a sparse MIP directly via Rglpk or highs
#'
#' Bypasses `ompr`/`ROI`: builds a plain sparse-matrix MIP once and hands it
#' straight to the chosen solver's native matrix interface. `ompr`'s own
#' objective-vector construction (`objective_function.linear_optimization_model`)
#' assigns coefficients into a `Matrix::sparseVector` one element at a time,
#' which re-sorts the whole structure on every insertion -- O(n) calls that
#' are individually cheap but collectively quadratic-ish, costing minutes on
#' problems with tens of thousands of terms. This path avoids that entirely.
#'
#' @param L numeric. Objective coefficients (length = n_vars).
#' @param A sparse matrix (`Matrix::sparseMatrix`). Constraint matrix
#'   (n_constraints x n_vars).
#' @param dir character vector. `"<="`, `">="`, or `"=="` per constraint row.
#' @param rhs numeric. Right-hand side per constraint row.
#' @param types character vector. `"B"` (binary), `"C"` (continuous), or
#'   `"I"` (integer) per variable.
#' @param lower,upper numeric. Variable bounds (length = n_vars).
#' @param sense `"min"` or `"max"`.
#' @param solver `"glpk"` or `"highs"`.
#' @return list(optimal, objective_value, solution, status).
#' @noRd
solve_direct <- function(L, A, dir, rhs, types, lower, upper, sense, solver) {
  if (!solver %in% c("glpk", "highs"))
    stop(sprintf("Unknown solver '%s'. Use \"glpk\" or \"highs\".", solver))

  if (solver == "glpk") {
    is_special <- types != "B"
    bnds <- if (any(is_special)) {
      list(lower = list(ind = which(is_special), val = lower[is_special]),
           upper = list(ind = which(is_special & is.finite(upper)),
                        val = upper[is_special & is.finite(upper)]))
    } else NULL
    r <- Rglpk::Rglpk_solve_LP(obj = L, mat = A, dir = dir, rhs = rhs, types = types,
                                max = identical(sense, "max"), bounds = bnds)
    list(optimal = r$status == 0, objective_value = r$optimum, solution = r$solution,
         status = if (r$status == 0) "optimal" else paste0("glpk_status_", r$status))
  } else {
    lhs2 <- ifelse(dir == "<=", -Inf, rhs)
    rhs2 <- ifelse(dir == ">=", Inf, rhs)
    types2 <- ifelse(types == "B", "I", types)
    r <- highs::highs_solve(L = L, lower = lower, upper = upper, A = A,
                             lhs = lhs2, rhs = rhs2, types = types2,
                             maximum = identical(sense, "max"))
    ok <- identical(tolower(r$status_message), "optimal")
    list(optimal = ok, objective_value = r$objective_value, solution = r$primal_solution,
         status = if (ok) "optimal" else r$status_message)
  }
}

validate_sf <- function(obj, name, id_col) {
  if (!inherits(obj, "sf"))
    stop(sprintf("`%s` must be an sf object.", name))
  if (is.null(id_col))
    stop(sprintf("`%s_id` cannot be NULL.", name))
  if (!id_col %in% names(obj))
    stop(sprintf("Column '%s' does not exist in `%s`.", id_col, name))
  ids <- obj[[id_col]]
  if (any(is.na(ids)))
    stop(sprintf("Column '%s' of `%s` contains NA values.", id_col, name))
  if (anyDuplicated(ids))
    stop(sprintf("Column '%s' of `%s` contains duplicate ids.", id_col, name))
  invisible(TRUE)
}

validate_cost_matrix <- function(od, from_id, to_id, dist, name = "matrix_OD") {
  if (!is.data.frame(od))
    stop(sprintf("`%s` must be a data.frame.", name))
  for (col in list(from_id, to_id, dist)) {
    if (is.null(col))
      stop(sprintf("A required column name is NULL in `%s`.", name))
    if (!col %in% names(od))
      stop(sprintf("Column '%s' does not exist in `%s`.", col, name))
    if (any(is.na(od[[col]])))
      stop(sprintf("Column '%s' of `%s` contains NA values.", col, name))
  }
  if (any(od[[dist]] < 0))
    stop(sprintf("Column '%s' of `%s` contains negative distances.", dist, name))
  if (nrow(od) < 1)
    stop(sprintf("`%s` must have at least 1 row.", name))
  invisible(TRUE)
}

od_to_matrix <- function(od, from_id, to_id, dist, cutoff = 1000, ids_from = NULL, ids_to = NULL) {
  if (is.null(ids_from)) ids_from <- unique(od[[from_id]])
  if (is.null(ids_to))   ids_to   <- unique(od[[to_id]])
  mat <- matrix(Inf, nrow = length(ids_from), ncol = length(ids_to),
                dimnames = list(ids_from, ids_to))
  od_valid <- od[od[[dist]] <= cutoff, ]
  idx_from <- match(od_valid[[from_id]], ids_from)
  idx_to   <- match(od_valid[[to_id]], ids_to)
  keep <- !is.na(idx_from) & !is.na(idx_to)
  mat[cbind(idx_from[keep], idx_to[keep])] <- od_valid[[dist]][keep]
  mat
}

replace_inf <- function(mat, big = NULL) {
  if (is.null(big)) {
    finite_vals <- mat[is.finite(mat)]
    big <- if (length(finite_vals) > 0) max(finite_vals) * 10 else 1e9
  }
  mat[is.infinite(mat)] <- big
  mat
}

make_coverage_matrix <- function(cost_matrix, service_radius) {
  bij <- (cost_matrix <= service_radius) * 1L
  storage.mode(bij) <- "integer"
  bij
}

set_weights <- function(sf_obj, id_col, weight_col = NULL, name = "object") {
  if (is.null(weight_col)) {
    sf_obj[["weight"]] <- 1
    return(sf_obj)
  }
  if (!weight_col %in% names(sf_obj))
    stop(sprintf("Weight column '%s' does not exist in `%s`.", weight_col, name))
  if (!is.numeric(sf_obj[[weight_col]]))
    stop(sprintf("Column '%s' of `%s` must be numeric.", weight_col, name))
  if (any(sf_obj[[weight_col]] < 0, na.rm = TRUE))
    stop(sprintf("Column '%s' of `%s` contains negative values.", weight_col, name))
  n_na <- sum(is.na(sf_obj[[weight_col]]))
  if (n_na > 0) {
    warning(sprintf("%d NA value(s) in column '%s' of `%s` replaced with 1.",
                     n_na, weight_col, name))
    sf_obj[[weight_col]][is.na(sf_obj[[weight_col]])] <- 1
  }
  sf_obj
}

extract_assignment <- function(y_vals, ids_from, ids_to, cost_mat) {
  n <- length(ids_from)
  demand_id   <- character(n)
  facility_id <- character(n)
  distance    <- numeric(n)
  for (i in seq_len(n)) {
    demand_id[i] <- ids_from[i]
    row_i <- y_vals[y_vals$i == i & round(y_vals$value) == 1, ]
    if (nrow(row_i) == 0) {
      facility_id[i] <- NA_character_
      distance[i] <- NA_real_
    } else {
      j <- row_i$j[1]
      facility_id[i] <- ids_to[j]
      distance[i] <- cost_mat[i, j]
    }
  }
  data.frame(demand_id = demand_id, facility_id = facility_id, distance = distance,
             stringsAsFactors = FALSE)
}

build_result <- function(model_type, solver_status, sf_selected, ...) {
  structure(
    c(list(model_type = model_type, solver_status = solver_status,
           sf_selected = sf_selected), list(...)),
    class = "llocalocal_result"
  )
}

validate_fixed_cost <- function(candidate, col, name = "candidate") {
  if (!col %in% names(candidate))
    stop(sprintf("Fixed-cost column '%s' does not exist in `%s`.", col, name))
  if (!is.numeric(candidate[[col]]))
    stop(sprintf("Fixed-cost column '%s' of `%s` must be numeric.", col, name))
  if (any(candidate[[col]] < 0, na.rm = TRUE))
    stop(sprintf("Fixed-cost column '%s' of `%s` contains negative values.", col, name))
  invisible(TRUE)
}

validate_capacity <- function(candidate, col, name = "candidate") {
  if (!col %in% names(candidate))
    stop(sprintf("Capacity column '%s' does not exist in `%s`.", col, name))
  if (!is.numeric(candidate[[col]]))
    stop(sprintf("Capacity column '%s' of `%s` must be numeric.", col, name))
  if (any(candidate[[col]] <= 0, na.rm = TRUE))
    stop(sprintf("Capacity column '%s' of `%s` must be strictly positive.", col, name))
  invisible(TRUE)
}

derive_competitor_baseline <- function(cost_mat_existing) {
  apply(cost_mat_existing, 1, min)
}

enumerate_breakpoints <- function(cost_mat_cand, baseline, t, P_B, max_breakpoints = 2000) {
  n_i <- nrow(cost_mat_cand); n_j <- ncol(cost_mat_cand)
  bp <- outer(seq_len(n_i), seq_len(n_j), function(i, j) {
    P_B + t * baseline[i] - t * cost_mat_cand[cbind(i, j)]
  })
  bp <- sort(unique(as.vector(bp)), decreasing = TRUE)
  if (length(bp) > max_breakpoints) {
    warning(sprintf(
      "%d breakpoints exceeds max_breakpoints = %d; subsampling to %d (result is approximate, not exact).",
      length(bp), max_breakpoints, max_breakpoints
    ))
    bp <- bp[round(seq(1, length(bp), length.out = max_breakpoints))]
  }
  bp
}
