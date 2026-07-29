test_that("pmaxcap finds the exact profit-maximizing price and site", {
  fx <- competition_fixture()
  res <- pmaxcap(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    competitor_price = 20, distance_cost_rate = 1, marginal_cost = 0,
    n_facilities = 1, max_breakpoints = 2000
  )
  # Breakpoints (P_B + baseline - d_ij): D1/D2 via C1 = 26, D3 via C1 = 17,
  # D1/D2 via C2 = 15, D3 via C2 = 21. Best is price=26, opening C1: captures
  # D1+D2 (weight 2, D3 excluded since its threshold 17 < 26). Profit = 26*2 = 52.
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$optimal_price, 26)
  expect_equal(res$covered_demand, 2)
  expect_equal(res$profit, 52)
})
