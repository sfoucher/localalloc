# Guards the fixtures themselves rather than the models.
#
# The OD distances in `helper-fixtures.R` are hard-coded so that the exact
# totals asserted elsewhere stay reproducible across platforms. The cost of
# hard-coding is that the numbers can silently drift away from the coordinates
# they were derived from -- which is precisely the defect these fixtures used to
# have. This file closes that loop: it recomputes the distances from the
# geometry and fails if the two no longer agree.
#
# The 2% tolerance absorbs the small differences between s2 and GEOS/lwgeom
# geodesic backends (and between their versions) while still catching any real
# edit to a coordinate -- the fixture's closest pair of distinct points is
# 530 m apart, so a genuine mistake moves a distance by far more than 2%.

# Note the comparison is element-wise on purpose. `expect_equal(tolerance =)`
# would NOT do: it tests a *mean* relative difference over the whole vector, so
# a single distance edited by 5% is diluted by its five correct neighbours and
# slips through. Checking the worst element is what actually catches a one-line
# edit.
expect_od_matches_geometry <- function(fx, od, from_layer, to_layer) {
  computed <- vapply(seq_len(nrow(od)), function(k) {
    i <- match(od$from_id[k], fx[[from_layer]]$id)
    j <- match(od$to_id[k],   fx[[to_layer]]$id)
    as.numeric(sf::st_distance(fx[[from_layer]][i, ], fx[[to_layer]][j, ]))
  }, numeric(1))
  rel <- abs(od$distance - computed) / computed
  k <- which.max(rel)
  expect_true(
    max(rel) < 0.02,
    info = sprintf(
      "worst mismatch %s -> %s: table says %g m, geometry says %g m (%.2f%% off)",
      od$from_id[k], od$to_id[k], od$distance[k], round(computed[k]), 100 * rel[k])
  )
}

test_that("mini_fixture OD distances match its Sherbrooke geometry", {
  fx <- mini_fixture()
  expect_od_matches_geometry(fx, fx$od_candidates, "demand", "candidate")
  expect_od_matches_geometry(fx, fx$od_existing,   "demand", "existing")
})

test_that("competition_fixture OD distances match its Sherbrooke geometry", {
  fx <- competition_fixture()
  expect_od_matches_geometry(fx, fx$od_candidates, "demand", "candidate")
  expect_od_matches_geometry(fx, fx$od_existing,   "demand", "existing")
})

test_that("mini_fixture_two_existing OD distances match its Sherbrooke geometry", {
  fx <- mini_fixture_two_existing(mini_fixture())
  expect_od_matches_geometry(fx, fx$od_existing, "demand", "existing")
})

test_that("fixture points really are in Sherbrooke", {
  # Sherbrooke's bounding box, taken from the shipped bixi_* case-study layers
  # with a small margin. Catches a sign flip or a transposed lon/lat, which
  # would otherwise leave the distances plausible but put the points in Asia.
  for (fx in list(mini_fixture(), competition_fixture())) {
    for (layer in c("demand", "candidate", "existing")) {
      xy <- sf::st_coordinates(fx[[layer]])
      expect_true(all(xy[, "X"] > -72.05 & xy[, "X"] < -71.75), label = layer)
      expect_true(all(xy[, "Y"] >   45.30 & xy[, "Y"] <   45.50), label = layer)
    }
  }
})
