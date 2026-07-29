# Ports localalloc's original data/data.Rdata (the Sherbrooke Bixi case
# study from Essai_MarieHelene.pdf, preserved at Task 2 as
# data-raw/legacy-data.Rdata) into llocalocal's naming convention. Run
# once; data-raw/legacy-data.Rdata can be deleted afterwards.

e <- new.env()
load("data-raw/legacy-data.Rdata", envir = e)

bixi_candidates     <- e$candidate_sites
bixi_demand         <- e$demand_pop
bixi_existing       <- e$existing_sites
bixi_od_candidates  <- e$matrix_D_Candidates
bixi_od_existing    <- e$matrix_D_ExistingSites

save(bixi_candidates, file = "data/bixi_candidates.rda", compress = "bzip2")
save(bixi_demand, file = "data/bixi_demand.rda", compress = "bzip2")
save(bixi_existing, file = "data/bixi_existing.rda", compress = "bzip2")
save(bixi_od_candidates, file = "data/bixi_od_candidates.rda", compress = "bzip2")
save(bixi_od_existing, file = "data/bixi_od_existing.rda", compress = "bzip2")
