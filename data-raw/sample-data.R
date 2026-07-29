# Generates the small synthetic dataset bundled as sample_demand /
# sample_candidates / sample_existing / sample_od_candidates /
# sample_od_existing. Not part of the built package -- run this script
# once (or whenever the sample data needs regenerating) to (re)produce
# data/*.rda via base save().

library(sf)

set.seed(42)

sample_demand <- st_as_sf(
  data.frame(
    id = sprintf("D%02d", 1:15),
    weight = sample(50:200, 15),
    x = runif(15, 0, 10),
    y = runif(15, 0, 10)
  ),
  coords = c("x", "y"), crs = 4326
)

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

sample_existing <- st_as_sf(
  data.frame(
    id = sprintf("E%02d", 1:3),
    x = runif(3, 0, 10),
    y = runif(3, 0, 10)
  ),
  coords = c("x", "y"), crs = 4326
)

.euclid_od <- function(a, b) {
  ca <- st_coordinates(a); cb <- st_coordinates(b)
  grid <- expand.grid(i = seq_len(nrow(ca)), j = seq_len(nrow(cb)))
  grid$distance <- sqrt((ca[grid$i, 1] - cb[grid$j, 1])^2 + (ca[grid$i, 2] - cb[grid$j, 2])^2)
  data.frame(from_id = a$id[grid$i], to_id = b$id[grid$j], distance = grid$distance)
}

sample_od_candidates <- .euclid_od(sample_demand, sample_candidates)
sample_od_existing   <- .euclid_od(sample_demand, sample_existing)

dir.create("data", showWarnings = FALSE)
save(sample_demand, file = "data/sample_demand.rda", compress = "bzip2")
save(sample_candidates, file = "data/sample_candidates.rda", compress = "bzip2")
save(sample_existing, file = "data/sample_existing.rda", compress = "bzip2")
save(sample_od_candidates, file = "data/sample_od_candidates.rda", compress = "bzip2")
save(sample_od_existing, file = "data/sample_od_existing.rda", compress = "bzip2")
