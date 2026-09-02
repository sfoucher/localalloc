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

test_that("dp rejects an OD table with no candidate-to-candidate pair", {
  # The mistake this guards against: handing dp() the demand-to-candidate OD
  # table that the other nine models take. Its `from_id`s are demand ids, so not
  # one cell of the pairwise matrix gets filled. Left unchecked this used to
  # solve "successfully" and report min_distance = 0 -- every pair collapsed to
  # distance 0 by replace_inf()'s big-M -- with an arbitrary set of p sites.
  fx <- mini_fixture()
  expect_error(
    dp(candidate = fx$candidate, candidate_id = "id",
       matrix_OD_candidates = fx$od_candidates, p_facilities = 2),
    "no candidate-to-candidate pair"
  )
})

test_that("dp warns when candidate pairs are missing from the OD table", {
  # Same three landmarks as the first test, but only 2 of the 3 pairs supplied:
  # {B,C} is absent entirely. replace_inf() will hand it max * 10 = 49500 m,
  # making it look like the best-dispersed pair in the set -- so the warning has
  # to fire before the caller trusts the answer.
  candidate <- sf::st_as_sf(
    data.frame(id = c("A", "B", "C"),
               x = c(-71.8929, -71.9276, -71.8560),
               y = c( 45.4042,  45.3800,  45.3680)),
    coords = c("x", "y"), crs = 4326
  )
  od <- data.frame(
    from_id = c("A", "B", "A", "C"),
    to_id   = c("B", "A", "C", "A"),
    distance = c(3819, 3819, 4950, 4950)
  )
  expect_warning(
    dp(candidate = candidate, candidate_id = "id",
       matrix_OD_candidates = od, p_facilities = 2),
    "1 of 3 candidate pair\\(s\\) \\(33.3%\\) are missing"
  )
})

test_that("dp warns specifically when only one direction of a pair is supplied", {
  # Every pair is measured, but each only once -- the shape a router that returns
  # one row per ordered pair produces. {B,C} exists as (C, B), which lands in the
  # lower triangle; only the upper triangle is read, so the pair still reads as
  # missing. The fix is to symmetrize, not to route more, and the warning says so.
  candidate <- sf::st_as_sf(
    data.frame(id = c("A", "B", "C"),
               x = c(-71.8929, -71.9276, -71.8560),
               y = c( 45.4042,  45.3800,  45.3680)),
    coords = c("x", "y"), crs = 4326
  )
  od <- data.frame(
    from_id = c("A", "A", "C"),
    to_id   = c("B", "C", "B"),
    distance = c(3819, 4950, 5750)
  )
  expect_warning(
    dp(candidate = candidate, candidate_id = "id",
       matrix_OD_candidates = od, p_facilities = 2),
    "1 of them are present in the opposite direction only"
  )
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
