test_that("dp maximizes the minimum pairwise distance between opened sites", {
  # Three Sherbrooke landmarks, deliberately spread across the city:
  #   A Centre-ville (-71.8929, 45.4042)
  #   B Campus principal UdeS (-71.9276, 45.3800)
  #   C Lennoxville (-71.8560, 45.3680)
  candidate <- sf::st_as_sf(
    data.frame(id = c("A", "B", "C"),
               x = c(-71.8929, -71.9276, -71.8560),
               y = c( 45.4042,  45.3800,  45.3680)),
    coords = c("x", "y"), crs = 4326
  )
  # Real geodesic distances between them, rounded to the metre (symmetric, so
  # each pair appears in both directions as the model expects).
  od <- data.frame(
    from_id = c("A", "B", "A", "C", "B", "C"),
    to_id   = c("B", "A", "C", "A", "C", "B"),
    distance = c(3819, 3819, 4950, 4950, 5750, 5750)
  )
  res <- dp(
    candidate = candidate, candidate_id = "id",
    matrix_OD_candidates = od, p_facilities = 2
  )
  # Pairs: {A,B} = 3819, {A,C} = 4950, {B,C} = 5750. Dispersion maximizes the
  # minimum pairwise distance, so it picks the two furthest-apart sites --
  # the campus and Lennoxville, at opposite ends of the city.
  expect_equal(sort(as.character(res$sf_selected$id)), c("B", "C"))
  expect_equal(res$min_distance, 5750)
})

test_that("dp requires at least 2 facilities", {
  candidate <- sf::st_as_sf(
    data.frame(id = c("A", "B"),
               x = c(-71.8929, -71.9276),
               y = c( 45.4042,  45.3800)),
    coords = c("x", "y"), crs = 4326
  )
  od <- data.frame(from_id = "A", to_id = "B", distance = 3819)
  expect_error(
    dp(candidate = candidate, candidate_id = "id", matrix_OD_candidates = od, p_facilities = 1),
    ">= 2"
  )
})
