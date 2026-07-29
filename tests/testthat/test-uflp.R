test_that("uflp opens the site maximizing total weighted distance (repulsion)", {
  fx <- mini_fixture()
  res <- uflp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    p_facilities = 1
  )
  # C1: 1+2+9=12 vs C2: 5+5+6=16 -- uflp maximizes, so C2 wins (opposite of
  # p_median's choice on the same fixture -- see test-p_median.R).
  expect_equal(as.character(res$sf_selected$id), "C2")
  expect_equal(res$total_cost, 16)
})
