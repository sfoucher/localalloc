test_that("cflp splits assignment across sites once capacity binds", {
  fx <- mini_fixture()
  res <- cflp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    candidate_fixed_cost = "fixed_cost",
    candidate_capacity = "capacity"
  )
  # C1 (CHUS) has capacity 1 and C2 (Lennoxville) capacity 3, against 3 units of
  # demand -- so C2 must open no matter what, and C1 can take at most one point.
  # Which one? The saving of using C1 over C2 is 4950-3471 = 1479 for D1,
  # 4389-3458 = 931 for D2, and negative for the campus D3. D1 gives the largest
  # saving, and 1479 exceeds C1's 1200 fixed cost, so opening C1 for D1 alone
  # pays off:
  #   D1->C1 (3471), D2->C2 (4389), D3->C2 (5750)
  #   fixed 1200+400 = 1600, transport 13610, total 15210
  # (vs 15489 for C2 alone, 15758 if C1 took D2, 18167 if it took D3.)
  expect_equal(sort(as.character(res$sf_selected$id)), c("C1", "C2"))
  expect_equal(res$total_cost, 15210)
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
