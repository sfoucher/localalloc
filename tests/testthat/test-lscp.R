test_that("lscp opens the single site that covers everyone", {
  fx <- mini_fixture()
  res <- lscp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    service_radius = 6000
  )
  # At a 6 km radius: C1 (CHUS) covers D1 (3471) and D2 (3458) but not the
  # campus D3 (7228). C2 (Lennoxville) covers D1 (4950), D2 (4389) and D3
  # (5750) -- all three. So opening C2 alone suffices; minimum facilities = 1.
  expect_s3_class(res, "localocal_result")
  expect_equal(res$model_type, "lscp")
  expect_equal(nrow(res$sf_selected), 1)
  expect_equal(as.character(res$sf_selected$id), "C2")
  expect_equal(res$n_open, 1)
})

test_that("lscp returns forced-open existing_sites in sf_selected", {
  fx <- mini_fixture()
  res <- lscp(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    matrix_OD_existing_site_from_id = "from_id",
    matrix_OD_existing_site_to_id = "to_id",
    matrix_OD_existing_site_dist = "distance",
    service_radius = 6000
  )
  # E2 (Cegep, 1975/1869/1905 m from the three demand points) is forced open and
  # already covers everyone well inside 6 km, so no candidate is needed -- but
  # sf_selected still reports the open facility.
  expect_equal(res$n_open, 1)
  expect_equal(as.character(res$sf_selected$id), "E2")
  expect_equal(res$sf_selected$source, "existing")
})

test_that("lscp errors when a demand point cannot be covered", {
  fx <- mini_fixture()
  # Drop the radius to 4 km. Downtown is still covered by C1 (3471, 3458), but
  # the campus D3 is now out of reach of both candidates (7228 and 5750), and
  # LSCP requires *total* coverage -- so this is infeasible by construction.
  # Compare with test-mclp.R, which runs the same radius and tolerates it.
  expect_error(
    lscp(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      service_radius = 4000
    ),
    "cannot be covered"
  )
})
