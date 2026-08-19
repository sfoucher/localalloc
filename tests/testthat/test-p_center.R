test_that("p_center opens the site minimizing the worst-case distance", {
  fx <- mini_fixture()
  res <- p_center(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    p_facilities = 1
  )
  # Worst-served demand point per candidate: C1 (CHUS) leaves the campus D3 at
  # 7228 m; C2 (Lennoxville) leaves it at 5750 m -> C2 wins. This diverges from
  # p_median's choice of C1 on the very same fixture (see test-p_median.R): C1
  # has the better total (14157 < 15089) but the worse maximum, and that
  # disagreement is the whole point of having both models.
  expect_equal(as.character(res$sf_selected$id), "C2")
  expect_equal(res$max_distance, 5750)
})
