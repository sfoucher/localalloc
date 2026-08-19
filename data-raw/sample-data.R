# Generates the small synthetic dataset bundled as sample_demand /
# sample_candidates / sample_existing / sample_od_candidates /
# sample_od_existing. Not part of the built package -- run this script
# once (or whenever the sample data needs regenerating) to (re)produce
# data/*.rda via base save().

library(sf)

# Fixed seed: the bundled .rda files must be reproducible, since examples, the
# vignette and the test fixtures all assert against these exact values. Changing
# the seed silently changes every expected number downstream.
set.seed(42)

# Distinct id prefixes per layer (D / C / E) so nothing collides -- the models
# treat a shared id between `candidate` and `existing_sites` as a duplicate to
# drop (or, in maxcap/pmaxcap, as an error).
#
# `weight` is the demand quantity a_i, read by every model that weights its
# objective (mclp, p_median, ufclp/cflp, maxcap/pmaxcap).
sample_demand <- st_as_sf(
  data.frame(
    id = sprintf("D%02d", 1:15),
    weight = sample(50:200, 15),
    x = runif(15, 0, 10),
    y = runif(15, 0, 10)
  ),
  coords = c("x", "y"), crs = 4326
)

# `fixed_cost` (f_j) and `capacity` (k_j) exist so the same layer can drive the
# fixed-charge models: ufclp() needs the first, cflp() both. At this seed the
# capacities total 2524 against 2045 of demand -- above it, so cflp()'s
# feasibility pre-check passes, but with little slack, which is what makes the
# capacity rows actually bind and the fixture worth testing against.
sample_candidates <- st_as_sf(
  data.frame(
    id = sprintf("C%02d", 1:12),
    fixed_cost = sample(10:50, 12),
    capacity = sample(100:400, 12),
    x = runif(12, 0, 10),
    y = runif(12, 0, 10)
  ),
  coords = c("x", "y"), crs = 4326
)

# Three existing sites: read as Required Facilities by lscp/mclp/p_median/
# p_center/uflp, and as the competitor's network by maxcap/pmaxcap.
sample_existing <- st_as_sf(
  data.frame(
    id = sprintf("E%02d", 1:3),
    x = runif(3, 0, 10),
    y = runif(3, 0, 10)
  ),
  coords = c("x", "y"), crs = 4326
)

# Complete (dense) long OD table between two point layers, straight-line
# distance. Every pair is present, so no model ever sees a missing-pair `Inf` in
# these fixtures. Coordinates are plain numbers on a 10x10 square -- the CRS is
# nominally 4326 but the distances are Euclidean in degrees, which is meaningless
# geographically and entirely sufficient as a fixture.
#
# `expand.grid()` + vectorised arithmetic rather than a loop: the whole cross
# product is built in one shot, and `a$id[grid$i]` / `b$id[grid$j]` label it with
# the `from_id`/`to_id`/`distance` column names the models default to.
.euclid_od <- function(a, b) {
  ca <- st_coordinates(a); cb <- st_coordinates(b)
  grid <- expand.grid(i = seq_len(nrow(ca)), j = seq_len(nrow(cb)))
  grid$distance <- sqrt((ca[grid$i, 1] - cb[grid$j, 1])^2 + (ca[grid$i, 2] - cb[grid$j, 2])^2)
  data.frame(from_id = a$id[grid$i], to_id = b$id[grid$j], distance = grid$distance)
}

# Both OD tables are demand-to-facility, matching what the models expect. Note
# there is no candidate-to-candidate table here, so dp() cannot be run on these
# fixtures as they stand -- its test builds its own.
sample_od_candidates <- .euclid_od(sample_demand, sample_candidates)
sample_od_existing   <- .euclid_od(sample_demand, sample_existing)

# One .rda per object (rather than a single multi-object file) because R's lazy
# data loading maps one file to one exported dataset name. `compress = "bzip2"`
# keeps the package under CRAN's size expectations.
dir.create("data", showWarnings = FALSE)
save(sample_demand, file = "data/sample_demand.rda", compress = "bzip2")
save(sample_candidates, file = "data/sample_candidates.rda", compress = "bzip2")
save(sample_existing, file = "data/sample_existing.rda", compress = "bzip2")
save(sample_od_candidates, file = "data/sample_od_candidates.rda", compress = "bzip2")
save(sample_od_existing, file = "data/sample_od_existing.rda", compress = "bzip2")
