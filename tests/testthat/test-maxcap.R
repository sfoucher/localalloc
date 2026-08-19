test_that("maxcap opens the site that captures the most demand from the competitor", {
  fx <- competition_fixture()
  res <- maxcap(
    demand = fx$demand, demand_id = "id",
    candidate = fx$candidate, candidate_id = "id",
    existing_sites = fx$existing, existing_sites_id = "id",
    matrix_OD_candidates = fx$od_candidates,
    matrix_OD_existing_site = fx$od_existing,
    p_facilities = 1
  )
  # The competitor (E1, Cegep de Sherbrooke) is central, so its baseline is
  # decent everywhere: D1 = 1975, D2 = 1869, D3 = 1905 m. A demand point is
  # captured only when our site is strictly closer than that.
  #   C1 (Parc Jacques-Cartier, downtown) captures D1 (1104 < 1975) and
  #     D2 (1665 < 1869), but not the campus D3 (4062 > 1905).
  #   C2 (Carrefour de l'Estrie, out west) captures only D3 (1451 < 1905);
  #     it is far too distant downtown (4708, 4804).
  # Two captured points beat one, so C1 is opened.
  expect_equal(as.character(res$sf_selected$id), "C1")
  expect_equal(res$covered_demand, 2)
})
