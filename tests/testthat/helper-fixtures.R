# Shared test fixtures, located on real Sherbrooke landmarks.
#
# The coordinates are genuine WGS 84 (EPSG:4326) positions in Sherbrooke, and
# the OD distances below are the *actual* geodesic distances between those
# points, rounded to the metre. Geometry and OD table therefore agree -- which
# matters, because a fixture whose distances contradict its coordinates makes
# every test comment a lie and hides real bugs.
#
# The distances are hard-coded rather than recomputed with `st_distance()` at
# run time on purpose: the tests assert exact totals (e.g. `total_cost ==
# 14157`), and letting a future s2/GEOS version shift a distance by a metre
# would break them on some platforms and not others. `test-fixtures.R` guards
# the other direction -- it recomputes the distances from the geometry and fails
# if anyone edits the coordinates without updating the tables here.
#
# Distances are in METRES throughout, so radii, cutoffs and fixed costs are too.

# ---------------------------------------------------------------------------
# mini_fixture(): 3 demand points, 2 candidates, 1 (or 2) existing site(s)
#
#   D1  Centre-ville, King Ouest / Wellington    (-71.8929, 45.4042)
#   D2  Marche de la Gare                        (-71.8887, 45.4001)
#   D3  Campus principal UdeS                    (-71.9276, 45.3800)
#   C1  CHUS Hopital Fleurimont                  (-71.8543, 45.4197)
#   C2  Lennoxville, Queen / College             (-71.8560, 45.3680)
#   E2  Cegep de Sherbrooke                      (-71.9075, 45.3897)
#
# Distance matrix (m):        D1      D2      D3     sum      max
#   C1 (CHUS)              3471    3458    7228   14157     7228
#   C2 (Lennoxville)       4950    4389    5750   15089     5750
#   E2 (Cegep)             1975    1869    1905    5749     1975
#
# The geography is what drives every expected result, so it is worth stating:
#   * D1 and D2 are 530 m apart downtown; D3 is ~4 km southwest, on the campus.
#   * C1 sits northeast of downtown -- closest to D1/D2 of the two candidates,
#     but by far the worst for D3 (7228 m, it is on the opposite side of town).
#   * C2 sits south in Lennoxville -- worse than C1 for D1/D2, better for D3.
#     So C1 wins on the *total* (14157 < 15089) while C2 wins on the *worst
#     case* (5750 < 7228). That split is exactly why p_median picks C1 and
#     p_center picks C2 on the same data.
#   * E2 (Cegep) is central, between downtown and the campus, and is nearer to
#     all three demand points than either candidate -- so whenever it is forced
#     open it absorbs all the demand.
# ---------------------------------------------------------------------------
mini_fixture <- function() {
  demand <- sf::st_as_sf(
    data.frame(id = c("D1", "D2", "D3"),
               x = c(-71.8929, -71.8887, -71.9276),
               y = c( 45.4042,  45.4001,  45.3800)),
    coords = c("x", "y"), crs = 4326
  )
  # Fixed costs and capacities are in the same order of magnitude as the
  # distances (metres), otherwise they would be rounding noise next to a
  # multi-kilometre trip and cflp/ufclp would stop discriminating between sites.
  candidate <- sf::st_as_sf(
    data.frame(id = c("C1", "C2"), fixed_cost = c(1200, 400), capacity = c(1, 3),
               x = c(-71.8543, -71.8560),
               y = c( 45.4197,  45.3680)),
    coords = c("x", "y"), crs = 4326
  )
  existing <- sf::st_as_sf(
    data.frame(id = c("E2"), x = c(-71.9075), y = c(45.3897)),
    coords = c("x", "y"), crs = 4326
  )
  od_candidates <- data.frame(
    from_id = rep(c("D1", "D2", "D3"), times = 2),
    to_id   = rep(c("C1", "C2"), each = 3),
    distance = c(3471, 3458, 7228,   4950, 4389, 5750)
  )
  od_existing <- data.frame(
    from_id = c("D1", "D2", "D3"),
    to_id   = c("E2", "E2", "E2"),
    distance = c(1975, 1869, 1905)
  )
  list(demand = demand, candidate = candidate, existing = existing,
       od_candidates = od_candidates, od_existing = od_existing)
}

# ---------------------------------------------------------------------------
# competition_fixture(): same 3 demand points, but `existing` now belongs to a
# COMPETITOR (maxcap/pmaxcap semantics), not to us.
#
#   C1  Parc Jacques-Cartier                     (-71.9020, 45.4118)
#   C2  Carrefour de l'Estrie                    (-71.9455, 45.3835)
#   E1  Cegep de Sherbrooke  (competitor)        (-71.9075, 45.3897)
#
# Distance matrix (m):        D1      D2      D3
#   C1 (Parc Jacques-Cartier)  1104    1665    4062
#   C2 (Carrefour de l'Estrie) 4708    4804    1451
#   E1 (Cegep, competitor)     1975    1869    1905
#
# The competitor sits in the middle of town, so it serves everyone acceptably.
# Each candidate beats it on one side only:
#   * C1 is downtown, so it undercuts the competitor for D1 (1104 < 1975) and
#     D2 (1665 < 1869) -- but not for D3 (4062 > 1905).
#   * C2 is out west by the campus, so it undercuts the competitor for D3
#     (1451 < 1905) only -- it is far worse for D1 and D2.
# Capturing 2 demand points beats capturing 1, which is why C1 wins.
# ---------------------------------------------------------------------------
competition_fixture <- function() {
  demand <- sf::st_as_sf(
    data.frame(id = c("D1", "D2", "D3"),
               x = c(-71.8929, -71.8887, -71.9276),
               y = c( 45.4042,  45.4001,  45.3800)),
    coords = c("x", "y"), crs = 4326
  )
  candidate <- sf::st_as_sf(
    data.frame(id = c("C1", "C2"), fixed_cost = c(0, 0),
               x = c(-71.9020, -71.9455),
               y = c( 45.4118,  45.3835)),
    coords = c("x", "y"), crs = 4326
  )
  existing <- sf::st_as_sf(
    data.frame(id = c("E1"), x = c(-71.9075), y = c(45.3897)),
    coords = c("x", "y"), crs = 4326
  )
  od_candidates <- data.frame(
    from_id = rep(c("D1", "D2", "D3"), times = 2),
    to_id   = rep(c("C1", "C2"), each = 3),
    distance = c(1104, 1665, 4062,   4708, 4804, 1451)
  )
  od_existing <- data.frame(
    from_id = c("D1", "D2", "D3"),
    to_id   = c("E1", "E1", "E1"),
    distance = c(1975, 1869, 1905)
  )
  list(demand = demand, candidate = candidate, existing = existing,
       od_candidates = od_candidates, od_existing = od_existing)
}

# Extra existing site used only by the p_facilities validation tests.
#   E3  Parc Jacques-Cartier                     (-71.9020, 45.4118)
# Distances to D1/D2/D3: 1104 / 1665 / 4062 m.
mini_fixture_two_existing <- function(fx) {
  fx$existing <- sf::st_as_sf(
    data.frame(id = c("E2", "E3"),
               x = c(-71.9075, -71.9020),
               y = c( 45.3897,  45.4118)),
    coords = c("x", "y"), crs = 4326
  )
  fx$od_existing <- data.frame(
    from_id = rep(c("D1", "D2", "D3"), times = 2),
    to_id   = rep(c("E2", "E3"), each = 3),
    distance = c(1975, 1869, 1905,   1104, 1665, 4062)
  )
  fx
}
