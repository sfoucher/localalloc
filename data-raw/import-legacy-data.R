# Ports the original data/data.Rdata of the pre-rewrite package (the
# Sherbrooke Bixi case study from Essai_MarieHelene.pdf, preserved at Task 2
# as data-raw/legacy-data.Rdata) into this package's naming convention. Run
# once; data-raw/legacy-data.Rdata can be deleted afterwards.

# Load into a throwaway environment rather than the global one: the legacy file
# carries whatever names the original project used, and `load()` would silently
# overwrite anything of that name in the session. Reading through `e$` also makes
# the old-name -> new-name mapping explicit below.
e <- new.env()
load("data-raw/legacy-data.Rdata", envir = e)

# The rename is the entire point of this script -- the objects themselves are
# copied through untouched. `matrix_D_*` are the long OD tables (note their
# distance column is `travel_time_p50`, not `distance`, so callers of the bixi
# data must pass `matrix_OD_candidates_dist = "travel_time_p50"`).
bixi_candidates     <- e$candidate_sites
bixi_demand         <- e$demand_pop
bixi_existing       <- e$existing_sites
bixi_od_candidates  <- e$matrix_D_Candidates
bixi_od_existing    <- e$matrix_D_ExistingSites

# One .rda per object, as R's lazy data loading requires; bzip2 because the OD
# tables (5,811 x 176 pairs) dominate the package's installed size.
save(bixi_candidates, file = "data/bixi_candidates.rda", compress = "bzip2")
save(bixi_demand, file = "data/bixi_demand.rda", compress = "bzip2")
save(bixi_existing, file = "data/bixi_existing.rda", compress = "bzip2")
save(bixi_od_candidates, file = "data/bixi_od_candidates.rda", compress = "bzip2")
save(bixi_od_existing, file = "data/bixi_od_existing.rda", compress = "bzip2")
