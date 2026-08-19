test_that("ufclp opens both sites when the tradeoff favors it", {
  fx <- mini_fixture()
  res <- ufclp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    candidate_fixed_cost = "fixed_cost"
  )
  # Fixed costs are 1200 (C1, CHUS) and 400 (C2, Lennoxville), in the same
  # metre units as the distances.
  #   Only C1: 1200 + (3471+3458+7228) = 1200 + 14157 = 15357
  #   Only C2:  400 + (4950+4389+5750) =  400 + 15089 = 15489
  #   Both   : 1600 + min-per-point    = 1600 + (3471+3458+5750) = 14279
  # Opening both wins: each site then serves only the demand it is good at --
  # C1 takes the two downtown points, C2 takes the campus -- and the 1600 m of
  # combined fixed cost is cheaper than the detour either site alone would
  # impose. Compare uflp/p_median, which have no fixed cost to trade off.
  expect_equal(sort(as.character(res$sf_selected$id)), c("C1", "C2"))
  expect_equal(res$fixed_cost_total, 1600)
  expect_equal(res$transport_cost_total, 12679)
  expect_equal(res$total_cost, 14279)
})
