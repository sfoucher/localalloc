test_that("mclp maximizes covered demand under a facility budget", {
  fx <- mini_fixture()
  res <- mclp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    service_radius = 4, p_facilities = 1
  )
  # At radius 4: C1 covers D1(1),D2(2) (weight 2); C2 covers nobody (5,5,6 all > 4).
  # Best single site = C1, covering D1+D2 (D3 stays uncovered -- that's fine for MCLP).
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$covered_demand, 2)
  expect_equal(res$n_open, 1)
})

test_that("mclp does not require full coverability, unlike lscp", {
  fx <- mini_fixture()
  # No candidate covers D3 at radius 4 (distances 9 and 6) -- must not error.
  expect_no_error(
    mclp(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      service_radius = 4, p_facilities = 1
    )
  )
})

test_that("mclp treats existing_sites as free coverage that doesn't consume the budget", {
  fx <- mini_fixture()
  res <- mclp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    matrix_OD_existing_site_from_id = "from_id",
    matrix_OD_existing_site_to_id = "to_id",
    matrix_OD_existing_site_dist = "distance",
    service_radius = 0.6, p_facilities = 1
  )
  # E2 is 0.5 from everyone (covers all 3 at radius 0.6); neither C1 (1,2,9)
  # nor C2 (5,5,6) covers anyone at this radius. Coverage is driven entirely
  # by the forced-open existing site; p_facilities=1 still requires opening
  # one candidate (contributing nothing), so n_open = 1 candidate + 1 existing = 2.
  expect_equal(res$covered_demand, 3)
  expect_equal(res$n_open, 2)
})
