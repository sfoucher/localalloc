test_that("dp maximizes the minimum pairwise distance between opened sites", {
  candidate <- sf::st_as_sf(
    data.frame(id = c("A", "B", "C"), x = c(0, 5, 12), y = c(0, 0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  od <- data.frame(
    from_id = c("A", "B", "A", "C", "B", "C"),
    to_id   = c("B", "A", "C", "A", "C", "B"),
    distance = c(5, 5, 3, 3, 8, 8)
  )
  res <- dp(
    candidate = candidate, candidate_id = "id",
    matrix_OD_candidates = od, p_facilities = 2
  )
  # Pairs: {A,B}=5, {A,C}=3, {B,C}=8 -- maximize the min -> pick {B,C}, D=8.
  expect_equal(sort(as.character(res$sf_selected$id)), c("B", "C"))
  expect_equal(res$min_distance, 8)
})

test_that("dp requires at least 2 facilities", {
  candidate <- sf::st_as_sf(
    data.frame(id = c("A", "B"), x = c(0, 5), y = c(0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  od <- data.frame(from_id = "A", to_id = "B", distance = 5)
  expect_error(
    dp(candidate = candidate, candidate_id = "id", matrix_OD_candidates = od, p_facilities = 1),
    ">= 2"
  )
})
