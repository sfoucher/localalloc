test_that("p_median opens the site minimizing total weighted distance", {
  fx <- mini_fixture()
  res <- p_median(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    p_facilities = 1
  )
  # C1 (CHUS): 3471+3458+7228 = 14157 m vs C2 (Lennoxville): 4950+4389+5750 =
  # 15089 m -> C1 wins. C1 is much better for the two downtown points and much
  # worse for the campus, and on the *total* the downtown gain prevails.
  # See test-p_center.R for the same fixture judged on the worst case instead.
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$total_cost, 14157)
  expect_equal(res$n_open, 1)
})

test_that("p_median forces existing_sites open and prefers them when closer", {
  fx <- mini_fixture()
  res <- p_median(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    matrix_OD_existing_site_from_id = "from_id",
    matrix_OD_existing_site_to_id = "to_id",
    matrix_OD_existing_site_dist = "distance",
    p_facilities = 1
  )
  # E2 (Cegep) is central: 1975/1869/1905 m, nearer than either candidate is to
  # any demand point, so every demand point is assigned to it for a total of
  # 5749 m. p_facilities counts total open facilities, so the single forced-open
  # existing site consumes the whole budget and no candidate is selected.
  expect_equal(res$total_cost, 5749)
  expect_equal(res$n_open, 1)
  expect_true(all(res$assignments$source == "existing"))
  # sf_selected lists every open facility: here only the existing site.
  expect_equal(nrow(res$sf_selected), 1)
  expect_equal(as.character(res$sf_selected$id), "E2")
  expect_equal(res$sf_selected$source, "existing")
})

test_that("p_median counts existing_sites against the p_facilities budget", {
  fx <- mini_fixture()
  res <- p_median(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    matrix_OD_existing_site_from_id = "from_id",
    matrix_OD_existing_site_to_id = "to_id",
    matrix_OD_existing_site_dist = "distance",
    p_facilities = 2
  )
  # p = 2 total = 1 existing (forced) + 1 candidate selected. E2 is still nearer
  # to all three demand points than any candidate, so it keeps serving everyone
  # and the extra candidate is a free rider (either C1 or C2) that changes
  # nothing: the total stays 5749 m.
  expect_equal(res$n_open, 2)
  expect_equal(nrow(res$sf_selected), 2)
  expect_equal(res$total_cost, 5749)
  # Both open facilities are returned, tagged by origin.
  expect_setequal(res$sf_selected$source, c("candidate", "existing"))
  expect_equal(
    as.character(res$sf_selected$id[res$sf_selected$source == "existing"]), "E2")
  expect_true(
    as.character(res$sf_selected$id[res$sf_selected$source == "candidate"]) %in% c("C1", "C2"))
  # `existing_sites` carries none of the candidate attributes -> NA-filled.
  expect_true(inherits(res$sf_selected, "sf"))
  expect_true(is.na(res$sf_selected$fixed_cost[res$sf_selected$source == "existing"]))
})

test_that("p_median rejects a p_facilities inconsistent with existing_sites", {
  fx <- mini_fixture()
  call_pm <- function(p, fixture = fx) {
    p_median(
      demand = fixture$demand, demand_id = "id",
      candidate = fixture$candidate, candidate_id = "id",
      existing_sites = fixture$existing, existing_sites_id = "id",
      matrix_OD_candidates = fixture$od_candidates,
      matrix_OD_existing_site = fixture$od_existing,
      matrix_OD_existing_site_from_id = "from_id",
      matrix_OD_existing_site_to_id = "to_id",
      matrix_OD_existing_site_dist = "distance",
      p_facilities = p
    )
  }
  # 2 candidates + 1 existing = 3 available.
  expect_error(call_pm(4), "cannot exceed the total number of available sites")
  expect_no_error(suppressMessages(call_pm(3)))

  # Two forced-open existing sites (E2 = Cegep, E3 = Parc Jacques-Cartier)
  # can't fit in a budget of 1.
  expect_error(call_pm(1, mini_fixture_two_existing(fx)),
               "cannot be lower than the number of `existing_sites`")
})

test_that("p_median excludes candidate/existing_sites overlap with a warning instead of erroring", {
  fx <- mini_fixture()
  fx$candidate$id[1] <- "E2"  # force a collision with mini_fixture()'s existing site id
  expect_warning(
    res <- p_median(
      demand = fx$demand, demand_id = "id",
      candidate = fx$candidate, candidate_id = "id",
      existing_sites = fx$existing, existing_sites_id = "id",
      matrix_OD_candidates = fx$od_candidates,
      matrix_OD_existing_site = fx$od_existing,
      matrix_OD_existing_site_from_id = "from_id",
      matrix_OD_existing_site_to_id = "to_id",
      matrix_OD_existing_site_dist = "distance",
      p_facilities = 2  # 1 existing (forced) + 1 candidate
    ),
    "already open"
  )
  # E2 appears once, as the existing site -- never as a selected candidate.
  cands <- as.character(res$sf_selected$id[res$sf_selected$source == "candidate"])
  expect_false("E2" %in% cands)
  expect_equal(sum(as.character(res$sf_selected$id) == "E2"), 1L)
})
