test_that("cflp splits assignment across sites once capacity binds", {
  fx <- mini_fixture()
  res <- cflp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    candidate_fixed_cost = "fixed_cost",
    candidate_capacity = "capacity"
  )
  # C1 capacity = 1: can only serve one demand unit -- the cheapest, D1 (dist 1).
  # D1->C1(1), D2->C2(5), D3->C2(6): fixed 3+1=4, transport 1+5+6=12, total=16.
  expect_equal(sort(as.character(res$sf_selected$id)), c("C1", "C2"))
  expect_equal(res$total_cost, 16)
  d1_row <- res$assignments[res$assignments$demand_id == "D1", ]
  expect_equal(d1_row$facility_id, "C1")
})

test_that("cflp errors when total capacity is below total demand", {
  fx <- mini_fixture()
  fx$candidate$capacity <- c(1, 1)  # total capacity 2 < total demand 3
  expect_error(
    cflp(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      candidate_fixed_cost = "fixed_cost",
      candidate_capacity = "capacity"
    ),
    "no feasible assignment"
  )
})
