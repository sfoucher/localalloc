test_that("lscp opens the single site that covers everyone", {
  fx <- mini_fixture()
  res <- lscp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    service_radius = 6
  )
  # At radius 6: C1 covers D1(1),D2(2) but not D3(9). C2 covers D1(5),D2(5),D3(6) -- all three.
  # So opening C2 alone suffices; minimum facilities = 1.
  expect_s3_class(res, "llocalocal_result")
  expect_equal(res$model_type, "lscp")
  expect_equal(nrow(res$sf_selected), 1)
  expect_equal(as.character(res$sf_selected$id), "C2")
  expect_equal(res$n_open, 1)
})

test_that("lscp errors when a demand point cannot be covered", {
  fx <- mini_fixture()
  expect_error(
    lscp(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      service_radius = 3
    ),
    "cannot be covered"
  )
})
