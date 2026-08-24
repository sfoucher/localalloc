test_that("validate_sf rejects non-sf input and missing/duplicate/NA ids", {
  df <- data.frame(id = c("a", "b"))
  expect_error(validate_sf(df, "x", "id"), "must be an sf object")

  # Centre-ville and Marche de la Gare; the geometry is irrelevant to these
  # validation paths, but keeping every fixture point in Sherbrooke avoids
  # stray coordinates that look meaningful and are not.
  pts <- sf::st_as_sf(data.frame(id = c("a", "a"),
                                 x = c(-71.8929, -71.8887), y = c(45.4042, 45.4001)),
                      coords = c("x", "y"), crs = 4326)
  expect_error(validate_sf(pts, "x", "id"), "duplicate")

  pts_na <- sf::st_as_sf(data.frame(id = c("a", NA),
                                    x = c(-71.8929, -71.8887), y = c(45.4042, 45.4001)),
                         coords = c("x", "y"), crs = 4326)
  expect_error(validate_sf(pts_na, "x", "id"), "NA values")
})

test_that("validate_cost_matrix rejects bad OD tables", {
  expect_error(validate_cost_matrix(list(), "from_id", "to_id", "distance"),
              "must be a data.frame")
  # A negative distance is rejected rather than clamped: it would make an
  # objective term negative and quietly turn a minimization into an incentive to
  # travel, so the input has to be wrong.
  od <- data.frame(from_id = "a", to_id = "b", distance = -1)
  expect_error(validate_cost_matrix(od, "from_id", "to_id", "distance"),
              "negative distances")
})

test_that("od_to_matrix builds a wide matrix with Inf beyond cutoff", {
  # c2 is present in the OD table but at 100, beyond the cutoff of 10, so it ends
  # up `Inf` -- indistinguishable from a pair that was never listed. That is the
  # intent: both mean "not usable", and every model reads `Inf` as "create no
  # decision variable for this pair".
  od <- data.frame(from_id = c("d1", "d1"), to_id = c("c1", "c2"),
                   distance = c(1, 100))
  mat <- od_to_matrix(od, "from_id", "to_id", "distance", cutoff = 10)
  expect_equal(mat["d1", "c1"], 1)
  expect_true(is.infinite(mat["d1", "c2"]))
})

test_that("od_to_matrix() with ids_from/ids_to handles ids absent from the OD table", {
  # The layers claim points the OD table says nothing about: d2 as an origin, c3
  # as a destination. Passing `ids_from`/`ids_to` is what forces the matrix to the
  # shape of the sf layers (2 x 3) rather than to whatever the table happens to
  # mention -- the models index it positionally, so the dimensions must match the
  # layers, and an unmentioned point simply gets an all-`Inf` row or column.
  od <- data.frame(from_id = c("d1", "d1"), to_id = c("c1", "c2"), distance = c(1, 100))
  mat <- od_to_matrix(od, "from_id", "to_id", "distance", cutoff = 10,
                       ids_from = c("d1", "d2"), ids_to = c("c1", "c2", "c3"))
  expect_equal(dim(mat), c(2, 3))
  expect_equal(mat["d1", "c1"], 1)
  expect_true(all(is.infinite(mat["d2", ])))
  expect_true(all(is.infinite(mat[, "c3"])))
})

test_that("replace_inf swaps Inf for a large finite value", {
  mat <- matrix(c(1, Inf, 2, 3), nrow = 2)
  out <- replace_inf(mat)
  expect_true(all(is.finite(out)))
  expect_equal(out[2, 1], 30)  # 10 * max(finite) = 10 * 3
})

test_that("make_coverage_matrix thresholds correctly", {
  # `matrix()` fills column-major, so this is row1 = (1, 10), row2 = (5, 2).
  # At a radius of 4 only the two entries <= 4 survive -- (1,1) and (2,2) -- and
  # `as.vector()` reads them back out column-major too, hence c(1, 0, 0, 1).
  mat <- matrix(c(1, 5, 10, 2), nrow = 2)
  bij <- make_coverage_matrix(mat, service_radius = 4)
  expect_equal(as.vector(bij), c(1L, 0L, 0L, 1L))
})

test_that("set_weights fills missing weights with 1 and validates the column", {
  # No weight column: the helper invents one filled with 1s, which is what makes
  # `demand_weight = NULL` mean "count points" rather than "no objective" in
  # mclp/p_median/ufclp/maxcap.
  pts <- sf::st_as_sf(data.frame(id = "a", x = -71.8929, y = 45.4042),
                      coords = c("x", "y"), crs = 4326)
  out <- set_weights(pts, "id", NULL, "x")
  expect_equal(out$weight, 1)

  # A negative weight is an error, not a warning: unlike an NA (which is filled
  # with 1) there is no defensible interpretation -- it would pay the objective
  # for *not* serving a point.
  pts$w <- -1
  expect_error(set_weights(pts, "id", "w", "x"), "negative")
})

test_that("build_result constructs a localloc_result with arbitrary extra fields", {
  # The `...` passthrough is what lets each model attach its own metrics
  # (`covered_demand`, `max_distance`, `profit`, ...) without a common schema.
  # `print.localloc_result()` is written to probe for them rather than assume.
  res <- build_result("dummy", "optimal", data.frame(id = 1), foo = 42)
  expect_s3_class(res, "localloc_result")
  expect_equal(res$foo, 42)
})

test_that("print.localloc_result runs without error", {
  # A smoke test on the print method, deliberately given a bare data.frame as
  # `sf_selected` and only one of the optional metrics: it checks that the method
  # survives a result object missing most of its fields, which is the situation
  # every model but one puts it in.
  res <- build_result("dummy", "optimal", data.frame(id = 1), total_cost = 5)
  expect_output(print(res), "DUMMY")
})

test_that("validate_fixed_cost and validate_capacity reject bad columns", {
  # Note the two thresholds differ: a fixed cost of 0 is fine (a free site),
  # whereas a capacity of 0 is not -- a site that can serve nobody has no reason
  # to be a candidate and would only make the model harder to diagnose.
  cand <- data.frame(id = "a", cost = -1, cap = 0)
  expect_error(validate_fixed_cost(cand, "cost"), "negative")
  expect_error(validate_capacity(cand, "cap"), "strictly positive")
})

test_that("derive_competitor_baseline takes the row-wise minimum", {
  mat <- matrix(c(1, 5, 2, 3), nrow = 2)
  # mat is filled column-major: row1 = (1, 2), row2 = (5, 3)
  # row-wise min -> c(1, 3)
  expect_equal(derive_competitor_baseline(mat), c(1, 3))
})

test_that("enumerate_breakpoints sorts, dedupes, and subsamples over the cap", {
  # Column-major again: col1 (candidate 1) = (4, 4, 5), col2 = (15, 15, 1).
  # A breakpoint is bp_ij = P_B + t*baseline_i - t*d_ij, i.e. the highest price at
  # which candidate j still wins demand i:
  #   i=1 (baseline 10): 20+10-4  = 26 | 20+10-15 = 15
  #   i=2 (baseline 10): 26            | 15
  #   i=3 (baseline  2): 20+2-5  = 17  | 20+2-1   = 21
  # Six pairs, four distinct values, returned descending -> c(26, 21, 17, 15).
  # The duplicates matter: identical prices would otherwise make pmaxcap() solve
  # the same MIP twice.
  cost_mat <- matrix(c(4, 4, 5, 15, 15, 1), nrow = 3)
  baseline <- c(10, 10, 2)
  bp <- enumerate_breakpoints(cost_mat, baseline, t = 1, P_B = 20, max_breakpoints = 2000)
  expect_equal(bp, c(26, 21, 17, 15))

  # Over the cap the list is subsampled and the caller warned, because the sweep
  # is then no longer exhaustive -- pmaxcap()'s result becomes an approximation.
  expect_warning(
    bp_capped <- enumerate_breakpoints(cost_mat, baseline, t = 1, P_B = 20, max_breakpoints = 2),
    "exceeds max_breakpoints"
  )
  expect_length(bp_capped, 2)
})
