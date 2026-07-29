test_that("ufclp opens both sites when the tradeoff favors it", {
  fx <- mini_fixture()
  res <- ufclp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    candidate_fixed_cost = "fixed_cost"
  )
  # Only C1: fixed 3 + transport(1+2+9)=12 -> 15.
  # Only C2: fixed 1 + transport(5+5+6)=16 -> 17.
  # Both open: fixed 3+1=4, transport min(1,5)+min(2,5)+min(9,6)=1+2+6=9 -> 13.
  # Both-open wins.
  expect_equal(sort(as.character(res$sf_selected$id)), c("C1", "C2"))
  expect_equal(res$fixed_cost_total, 4)
  expect_equal(res$transport_cost_total, 9)
  expect_equal(res$total_cost, 13)
})
