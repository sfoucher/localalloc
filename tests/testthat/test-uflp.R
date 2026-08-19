test_that("uflp opens the site maximizing total weighted distance (repulsion)", {
  fx <- mini_fixture()
  res <- uflp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    p_facilities = 1
  )
  # C1 (CHUS): 14157 m vs C2 (Lennoxville): 15089 m -- uflp maximizes, so C2
  # wins, the opposite of p_median's choice on the same fixture (see
  # test-p_median.R). Same sparse model, same data, only `sense` differs.
  expect_equal(as.character(res$sf_selected$id), "C2")
  expect_equal(res$total_cost, 15089)
})
