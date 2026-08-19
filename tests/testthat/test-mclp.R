test_that("mclp maximizes covered demand under a facility budget", {
  fx <- mini_fixture()
  res <- mclp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    service_radius = 4000, p_facilities = 1
  )
  # At a 4 km radius: C1 (CHUS) covers downtown D1 (3471) and D2 (3458), weight
  # 2. C2 (Lennoxville) covers nobody -- 4950, 4389 and 5750 are all beyond
  # 4 km. Best single site = C1; the campus D3 stays uncovered, which is
  # perfectly legal for MCLP.
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$covered_demand, 2)
  expect_equal(res$n_open, 1)
})

test_that("mclp does not require full coverability, unlike lscp", {
  fx <- mini_fixture()
  # Same 4 km radius as the lscp error test: no candidate reaches the campus D3
  # (7228 and 5750). lscp refuses this; mclp must simply leave D3 uncovered.
  expect_no_error(
    mclp(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      service_radius = 4000, p_facilities = 1
    )
  )
})

test_that("mclp counts forced-open existing_sites against the p_facilities budget", {
  fx <- mini_fixture()
  run <- function(p) {
    mclp(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      existing_sites = fx$existing, existing_sites_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      matrix_OD_existing_site = fx$od_existing,
      matrix_OD_existing_site_from_id = "from_id",
      matrix_OD_existing_site_to_id = "to_id",
      matrix_OD_existing_site_dist = "distance",
      service_radius = 2000, p_facilities = p
    )
  }
  # A tight 2 km radius isolates the existing site: E2 (Cegep) is central and
  # covers all three demand points (1975, 1869, 1905), while both candidates are
  # more than 3.4 km from every demand point and so cover nobody. Coverage is
  # therefore driven entirely by the forced-open existing site.
  res1 <- run(1)
  # p = 1 total is fully consumed by the existing site: no candidate opens,
  # and sf_selected holds that existing site alone.
  expect_equal(res1$covered_demand, 3)
  expect_equal(res1$n_open, 1)
  expect_equal(as.character(res1$sf_selected$id), "E2")
  expect_equal(res1$sf_selected$source, "existing")

  res2 <- run(2)
  # p = 2 total = 1 existing + 1 candidate (which adds no coverage here).
  expect_equal(res2$covered_demand, 3)
  expect_equal(res2$n_open, 2)
  expect_equal(nrow(res2$sf_selected), 2)
  expect_setequal(res2$sf_selected$source, c("candidate", "existing"))
})
