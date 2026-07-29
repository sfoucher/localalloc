test_that("maxcap opens the site that captures the most demand from the competitor", {
  fx <- competition_fixture()
  res <- maxcap(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    p_facilities = 1
  )
  # Baseline (nearest existing E1): D1=10, D2=10, D3=2.
  # C1 diverts D1(4<10) and D2(4<10) but not D3(5>=2). C2 diverts only D3(1<2).
  # Opening C1 captures weight 2 (D1+D2), beating C2's weight 1 (D3).
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$covered_demand, 2)
})
