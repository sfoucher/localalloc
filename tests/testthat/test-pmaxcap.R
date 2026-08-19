test_that("pmaxcap finds the exact profit-maximizing price and site", {
  fx <- competition_fixture()
  res <- pmaxcap(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    competitor_price = 3000, distance_cost_rate = 1, marginal_cost = 0,
    n_facilities = 1, max_breakpoints = 2000
  )
  # Price and distance share one unit here (distance_cost_rate = 1), so the
  # competitor's 3000 is on the same scale as the metre distances -- otherwise
  # undercutting on price would swamp any distance advantage and the tradeoff
  # would vanish.
  #
  # A demand point is captured while price <= P_B + baseline_i - d_ij:
  #   via C1: D1 3000+1975-1104 = 3871 | D2 3000+1869-1665 = 3204 | D3 843
  #   via C2: D1  267 | D2   65 | D3 3000+1905-1451 = 3454
  # Profit = price x captured weight, so each breakpoint is a real candidate:
  #   3871 -> C1 captures D1 only        -> 3871
  #   3454 -> C2 captures D3 only        -> 3454
  #   3204 -> C1 captures D1 + D2        -> 6408   <- best
  #    843 -> C1 captures all three      -> 2529
  # Charging less to capture the campus too is a net loss: the third point does
  # not pay for the price cut needed to reach it.
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$optimal_price, 3204)
  expect_equal(res$covered_demand, 2)
  expect_equal(res$profit, 6408)
})
