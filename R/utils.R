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
    # glpk understands "B" natively and implies the bounds [0, 1] for it, so only
    # the non-binary variables ("C"/"I", e.g. p_center's Z) need explicit bounds.
    # Passing bounds for a "B" variable would be redundant at best; skip them.
    is_special <- types != "B"
    bnds <- if (any(is_special)) {
      list(lower = list(ind = which(is_special), val = lower[is_special]),
           # An infinite upper bound cannot be sent as a number: leave those
           # variables out of the `upper` list, which glpk reads as "unbounded".
           upper = list(ind = which(is_special & is.finite(upper)),
                        val = upper[is_special & is.finite(upper)]))
    } else NULL
    r <- Rglpk::Rglpk_solve_LP(obj = L, mat = A, dir = dir, rhs = rhs, types = types,
                                max = identical(sense, "max"), bounds = bnds)
    list(optimal = r$status == 0, objective_value = r$optimum, solution = r$solution,
         status = if (r$status == 0) "optimal" else paste0("glpk_status_", r$status))
  } else {
    # highs takes no `dir` vector: every constraint is a two-sided range
    # lhs <= A x <= rhs, so each direction is encoded by relaxing one side to
    # infinity. "<=" becomes [-Inf, rhs], ">=" becomes [rhs, Inf], and "=="
    # keeps rhs on both sides (neither ifelse fires), pinning the row exactly.
    lhs2 <- ifelse(dir == "<=", -Inf, rhs)
    rhs2 <- ifelse(dir == ">=", Inf, rhs)
    # highs has no binary type: an integer variable bounded to [0, 1] by the
    # caller's `lower`/`upper` is equivalent.
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

# Long OD table -> wide cost matrix d_ij (rows = demand, cols = facilities).
#
# The `Inf` fill is the load-bearing part: a cell stays `Inf` when the OD table
# has no row for that (demand, facility) pair *or* when the pair's distance
# exceeds `cutoff`. Downstream, the models read `Inf` as "this pair is not
# allowed" and never create a decision variable for it
# (`which(is.finite(cost_mat), arr.ind = TRUE)`), which is what keeps the MIP
# sparse. `Inf` is used rather than NA precisely because it also compares
# correctly against a radius: `Inf <= service_radius` is FALSE.
#
# `ids_from`/`ids_to` should always be passed by the callers so that row/column
# order matches the `sf` layers' row order -- the models index the matrix
# positionally and the ids are recovered by position afterwards.
od_to_matrix <- function(od, from_id, to_id, dist, cutoff = 1000, ids_from = NULL, ids_to = NULL) {
  if (is.null(ids_from)) ids_from <- unique(od[[from_id]])
  if (is.null(ids_to))   ids_to   <- unique(od[[to_id]])
  mat <- matrix(Inf, nrow = length(ids_from), ncol = length(ids_to),
                dimnames = list(ids_from, ids_to))
  # Drop over-cutoff pairs before scattering: whatever is dropped here keeps its
  # `Inf` and therefore gets no decision variable later.
  od_valid <- od[od[[dist]] <= cutoff, ]
  # Map the OD table's ids onto matrix positions. `match()` returns NA for ids
  # absent from the sf layers (an OD table may cover more points than are being
  # modelled); those rows are discarded by `keep`.
  idx_from <- match(od_valid[[from_id]], ids_from)
  idx_to   <- match(od_valid[[to_id]], ids_to)
  keep <- !is.na(idx_from) & !is.na(idx_to)
  # Two-column-matrix indexing scatters all remaining distances in one shot.
  mat[cbind(idx_from[keep], idx_to[keep])] <- od_valid[[dist]][keep]
  mat
}

# Swap `Inf` for a large finite value ("big-M"): only for models that must keep
# every pair in the formulation and instead make forbidden pairs so expensive
# the optimiser never chooses them. Default M = 10x the largest real distance,
# large enough to dominate any feasible alternative yet small enough to avoid
# the numerical trouble a literal `Inf` coefficient would cause in the solver.
replace_inf <- function(mat, big = NULL) {
  if (is.null(big)) {
    finite_vals <- mat[is.finite(mat)]
    big <- if (length(finite_vals) > 0) max(finite_vals) * 10 else 1e9
  }
  mat[is.infinite(mat)] <- big
  mat
}

# Cost matrix -> binary coverage matrix b_ij used by LSCP/MCLP:
# b_ij = 1 if facility j is within `service_radius` of demand i, else 0.
# This is where "distance" stops mattering to those two models -- they only ever
# ask *whether* a pair is covered, not by how much. `Inf` cells (missing or
# over-cutoff pairs) fall out as 0 automatically.
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

# Decode the solver's Y block back into one row per demand point.
#
# `y_vals` is the sparse triplet form of the assignment variables: one row per
# *allowed* (i, j) pair, with `value` the solver's answer for Y_ij. Because the
# models constrain `sum_j Y_ij == 1`, exactly one pair per demand point should
# come back as 1 -- that pair names the facility serving i.
#
# `round()` is required: MIP solvers return 0.9999999 rather than 1 for a
# variable they consider integral, so an `== 1` test on the raw value would
# silently report the point as unassigned.
extract_assignment <- function(y_vals, ids_from, ids_to, cost_mat) {
  n <- length(ids_from)
  demand_id   <- character(n)
  facility_id <- character(n)
  distance    <- numeric(n)
  for (i in seq_len(n)) {
    demand_id[i] <- ids_from[i]
    row_i <- y_vals[y_vals$i == i & round(y_vals$value) == 1, ]
    if (nrow(row_i) == 0) {
      # No Y_ij = 1 for this demand point. Should not happen on an optimal
      # solve, so this is a defensive branch for non-optimal statuses.
      facility_id[i] <- NA_character_
      distance[i] <- NA_real_
    } else {
      # `[1]` guards against a degenerate tie; the id is recovered by position,
      # `ids_to` spanning candidates then existing sites in that order.
      j <- row_i$j[1]
      facility_id[i] <- ids_to[j]
      distance[i] <- cost_mat[i, j]
    }
  }
  data.frame(demand_id = demand_id, facility_id = facility_id, distance = distance,
             stringsAsFactors = FALSE)
}

#' Assemble the `sf_selected` output layer
#'
#' `sf_selected` lists *every open facility*, not just the candidates the
#' solver picked: forced-open `existing_sites` are appended, tagged by a
#' `source` column ("candidate" / "existing"). Only for models where
#' `existing_sites` are our own Required Facilities -- NOT for
#' `maxcap()`/`pmaxcap()`, where they belong to a competitor.
#'
#' The two layers rarely line up, so they are reconciled first: the existing
#' id column is renamed to `candidate_id`, ids are coerced to character if
#' their classes differ, `existing_sites` is reprojected to `candidate`'s CRS,
#' the geometry columns are given a common name, and any attribute carried by
#' only one layer becomes `NA` on the other.
#'
#' @param ids_selected character. Ids of the candidates the model opened.
#' @noRd
bind_selected_sites <- function(candidate, candidate_id, ids_selected,
                                 existing_sites = NULL, existing_sites_id = NULL) {
  # Compare as character: `ids_selected` is always character, while the layer's
  # own id column may be integer or factor.
  sel <- candidate[as.character(candidate[[candidate_id]]) %in% ids_selected, , drop = FALSE]
  sel[["source"]] <- rep("candidate", nrow(sel))
  if (is.null(existing_sites) || nrow(existing_sites) == 0) return(sel)

  ex <- existing_sites
  # If `existing_sites` already has a column named like the candidate id column
  # but the actual id lives elsewhere, that column would collide with the rename
  # below -- drop it first.
  if (!identical(existing_sites_id, candidate_id) && candidate_id %in% names(ex))
    ex[[candidate_id]] <- NULL
  names(ex)[names(ex) == existing_sites_id] <- candidate_id
  # `rbind()` on mismatched id classes (e.g. integer vs character) would coerce
  # unpredictably; force both to character instead.
  if (!identical(class(sel[[candidate_id]]), class(ex[[candidate_id]]))) {
    sel[[candidate_id]] <- as.character(sel[[candidate_id]])
    ex[[candidate_id]]  <- as.character(ex[[candidate_id]])
  }
  ex[["source"]] <- rep("existing", nrow(ex))

  crs_sel <- sf::st_crs(sel); crs_ex <- sf::st_crs(ex)
  if (!is.na(crs_sel) && !is.na(crs_ex) && crs_sel != crs_ex)
    ex <- sf::st_transform(ex, crs_sel)

  geom_sel <- attr(sel, "sf_column")
  geom_ex  <- attr(ex, "sf_column")
  if (!identical(geom_sel, geom_ex)) {
    names(ex)[names(ex) == geom_ex] <- geom_sel
    attr(ex, "sf_column") <- geom_sel
  }
  # Reconcile differing attribute sets: any column present on only one layer is
  # NA-filled on the other, then columns are put in a common order so `rbind()`
  # lines them up.
  for (col in setdiff(names(sel), names(ex))) ex[[col]] <- NA
  for (col in setdiff(names(ex), names(sel))) sel[[col]] <- NA
  ex <- ex[, names(sel), drop = FALSE]

  rbind(sel, ex)
}

build_result <- function(model_type, solver_status, sf_selected, ...) {
  structure(
    c(list(model_type = model_type, solver_status = solver_status,
           sf_selected = sf_selected), list(...)),
    class = "localloc_result"
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

# MAXCAP/PMAXCAP: for each demand point, the distance to its *nearest competitor*
# site. This is the baseline our own candidates have to beat to capture that
# demand -- row minimum of the demand-to-competitor cost matrix.
derive_competitor_baseline <- function(cost_mat_existing) {
  apply(cost_mat_existing, 1, min)
}

# MAXCAP/PMAXCAP: enumerate the candidate prices worth testing.
#
# Demand point i prefers our facility j over the competitor when our delivered
# cost is lower, i.e. P + t*d_ij <= P_B + t*baseline_i, so the highest price at
# which we still win i from j is P = P_B + t*baseline_i - t*d_ij. Profit is
# piecewise-constant in P and only changes as P crosses one of those values, so
# the optimum is attained at one of them -- there is no need to search a
# continuum of prices, only this finite set.
enumerate_breakpoints <- function(cost_mat_cand, baseline, t, P_B, max_breakpoints = 2000) {
  n_i <- nrow(cost_mat_cand); n_j <- ncol(cost_mat_cand)
  bp <- outer(seq_len(n_i), seq_len(n_j), function(i, j) {
    P_B + t * baseline[i] - t * cost_mat_cand[cbind(i, j)]
  })
  # Descending order so the caller sweeps from the most profitable price down.
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
