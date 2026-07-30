test_that("p_median opens the site minimizing total weighted distance", {
  fx <- mini_fixture()
  res <- p_median(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    p_facilities = 1
  )
  # C1: 1+2+9=12 vs C2: 5+5+6=16 -> C1 wins.
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$total_cost, 12)
  expect_equal(res$n_open, 1)
})

test_that("p_median forces existing_sites open and prefers them when closer", {
  fx <- mini_fixture()
  res <- p_median(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    matrix_OD_existing_site_from_id = "from_id",
    matrix_OD_existing_site_to_id = "to_id",
    matrix_OD_existing_site_dist = "distance",
    p_facilities = 1
  )
  # E2 is 0.5 from everyone -- always cheaper than any candidate, so every
  # demand point is assigned to E2 regardless of which candidate opens.
  expect_equal(res$total_cost, 1.5)
  expect_equal(res$n_open, 2)  # 1 candidate (forced budget) + 1 existing (forced open)
  expect_true(all(res$assignments$source == "existing"))
})

test_that("p_median excludes candidate/existing_sites overlap with a warning instead of erroring", {
  fx <- mini_fixture()
  fx$candidate$id[1] <- "E2"  # force a collision with mini_fixture()'s existing site id
  expect_warning(
    res <- p_median(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      existing_sites = fx$existing, existing_sites_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      matrix_OD_existing_site = fx$od_existing,
      matrix_OD_existing_site_from_id = "from_id",
      matrix_OD_existing_site_to_id = "to_id",
      matrix_OD_existing_site_dist = "distance",
      p_facilities = 1
    ),
    "already open"
  )
  expect_false("E2" %in% as.character(res$sf_selected$id))
})
