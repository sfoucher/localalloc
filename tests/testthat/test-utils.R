test_that("validate_sf rejects non-sf input and missing/duplicate/NA ids", {
  df <- data.frame(id = c("a", "b"))
  expect_error(validate_sf(df, "x", "id"), "must be an sf object")

  pts <- sf::st_as_sf(data.frame(id = c("a", "a"), x = 1:2, y = 1:2),
                      coords = c("x", "y"), crs = 4326)
  expect_error(validate_sf(pts, "x", "id"), "duplicate")

  pts_na <- sf::st_as_sf(data.frame(id = c("a", NA), x = 1:2, y = 1:2),
                         coords = c("x", "y"), crs = 4326)
  expect_error(validate_sf(pts_na, "x", "id"), "NA values")
})

test_that("validate_cost_matrix rejects bad OD tables", {
  expect_error(validate_cost_matrix(list(), "from_id", "to_id", "distance"),
              "must be a data.frame")
  od <- data.frame(from_id = "a", to_id = "b", distance = -1)
  expect_error(validate_cost_matrix(od, "from_id", "to_id", "distance"),
              "negative distances")
})

test_that("od_to_matrix builds a wide matrix with Inf beyond cutoff", {
  od <- data.frame(from_id = c("d1", "d1"), to_id = c("c1", "c2"),
                   distance = c(1, 100))
  mat <- od_to_matrix(od, "from_id", "to_id", "distance", cutoff = 10)
  expect_equal(mat["d1", "c1"], 1)
  expect_true(is.infinite(mat["d1", "c2"]))
})

test_that("replace_inf swaps Inf for a large finite value", {
  mat <- matrix(c(1, Inf, 2, 3), nrow = 2)
  out <- replace_inf(mat)
  expect_true(all(is.finite(out)))
  expect_equal(out[2, 1], 30)  # 10 * max(finite) = 10 * 3
})

test_that("make_coverage_matrix thresholds correctly", {
  mat <- matrix(c(1, 5, 10, 2), nrow = 2)
  bij <- make_coverage_matrix(mat, service_radius = 4)
  expect_equal(as.vector(bij), c(1L, 0L, 0L, 1L))
})

test_that("set_weights fills missing weights with 1 and validates the column", {
  pts <- sf::st_as_sf(data.frame(id = "a", x = 1, y = 1), coords = c("x", "y"), crs = 4326)
  out <- set_weights(pts, "id", NULL, "x")
  expect_equal(out$weight, 1)

  pts$w <- -1
  expect_error(set_weights(pts, "id", "w", "x"), "negative")
})

test_that("build_result constructs an llocalocal_result with arbitrary extra fields", {
  res <- build_result("dummy", "optimal", data.frame(id = 1), foo = 42)
  expect_s3_class(res, "llocalocal_result")
  expect_equal(res$foo, 42)
})

test_that("print.llocalocal_result runs without error", {
  res <- build_result("dummy", "optimal", data.frame(id = 1), total_cost = 5)
  expect_output(print(res), "DUMMY")
})

test_that("validate_fixed_cost and validate_capacity reject bad columns", {
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
  cost_mat <- matrix(c(4, 4, 5, 15, 15, 1), nrow = 3)
  baseline <- c(10, 10, 2)
  bp <- enumerate_breakpoints(cost_mat, baseline, t = 1, P_B = 20, max_breakpoints = 2000)
  expect_equal(bp, c(26, 21, 17, 15))

  expect_warning(
    bp_capped <- enumerate_breakpoints(cost_mat, baseline, t = 1, P_B = 20, max_breakpoints = 2),
    "exceeds max_breakpoints"
  )
  expect_length(bp_capped, 2)
})
