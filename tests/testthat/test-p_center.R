test_that("p_center opens the site minimizing the worst-case distance", {
  fx <- mini_fixture()
  res <- p_center(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    p_facilities = 1
  )
  # C1 max = 9 (D3); C2 max = 6 (D3) -> C2 wins (this diverges from
  # p_median's choice of C1 -- see test-p_median.R -- which is the point
  # of having both models).
  expect_equal(as.character(res$sf_selected$id), "C2")
  expect_equal(res$max_distance, 6)
})
