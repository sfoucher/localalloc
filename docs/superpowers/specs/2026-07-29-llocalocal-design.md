# llocalocal — design

Date: 2026-07-29

## Summary

`llocalocal` is a clean rewrite of the `localalloc` package: four
facility-location ILP models (LSCP, MCLP, P-Center, P-Median) built on
`ompr`/`ompr.roi`/`ROI.plugin.glpk`. It replaces `localalloc` in place in this
repo. Same solver stack and model math as `localalloc` (that part was
correct); the rewrite fixes plumbing bugs (missing `.build_result()`,
unexported functions, a `poids_demand`/`weight_demand` typo, empty test
file) and unifies all four functions onto one input contract.

## Package identity

- Name: `llocalocal`
- Author/maintainer: Samuel Foucher <samuel.foucher@gmail.com> (`aut`, `cre`)
- License: MIT + file LICENSE
- Language: English (roxygen docs, `stop()`/`warning()` messages)
- Location: this repo root, replacing the current `localalloc` scaffold
  (`DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `Package.zip`, `localalloc.Rproj`
  removed/rewritten). `Essai_MarieHelene.pdf` stays, added to
  `.Rbuildignore`.
- Version control: local git only (already initialized, baseline commit
  made). User creates the private GitHub repo and pushes; not done by
  Claude.

## Public API

All four exported functions share one input contract:

- `demand` (`sf` POINT) + `demand_id` + `demand_weight` (optional, default
  weight 1)
- `candidate` (`sf` POINT) + `candidate_id` + `candidate_weight` (optional)
- `existing_sites` (`sf` POINT, optional) + `existing_sites_id` +
  `existing_sites_weight` — when supplied, these facilities are forced open
  in the ILP ("Required Facilities") rather than competing for the budget
- `matrix_OD_candidates` (long `data.frame`: from_id/to_id/distance columns,
  names configurable) and, when `existing_sites` is supplied,
  `matrix_OD_existing_site`
- `cutoff_distance` — distance beyond which a demand-facility pair is pruned
- solver = `"glpk"` (default, via `ROI.plugin.glpk`)

Per-model knobs on top of the shared contract:

- `lscp()` — `service_radius`; minimizes facility count s.t. full coverage
- `mclp()` — `service_radius` + `p_facilities`; maximizes weighted demand
  covered with a fixed facility budget
- `p_center()` — `p_facilities`; minimizes the worst-case (max) assigned
  distance
- `p_median()` — `p_facilities`; minimizes total weighted assigned distance

All four return an S3 object of class `llocalocal_result` (fields:
`model_type`, `solver_status`, `sf_selected`, assignment table, and
model-appropriate stats such as `total_cost` or `max_distance`), printed via
one shared `print.llocalocal_result()`.

## Internal architecture

- `R/lscp.R`, `R/mclp.R`, `R/p_center.R`, `R/p_median.R` — one exported
  function each. Each is: validate inputs → build the merged
  candidate+existing cost matrix via shared helpers → build the `ompr`
  model (binary `X[j]`, and for assignment models `Y[i,j]`) → solve via
  `ompr.roi::with_ROI(solver)` → hand off to a shared `build_result()`.
- `R/utils.R` — internal (`.`-prefixed or `@noRd`) helpers used by all four:
  - `validate_sf()` / `validate_cost_matrix()` — shape/NA/duplicate-id
    checks, applied identically across all four models (today only
    `p_median` got full validation — this is the actual bug fix, not just a
    rename)
  - `od_to_matrix()` — long OD `data.frame` → wide matrix, `Inf` for
    missing/over-cutoff pairs
  - `replace_inf()` — swaps `Inf` for a large finite value for solver
    compatibility
  - `make_coverage_matrix()` — distance matrix → binary coverage matrix
    (LSCP/MCLP)
  - `set_weights()` — fills missing weight columns with 1
  - `extract_assignment()` — `ompr::get_solution()` output → assignment
    `data.frame`
  - `build_result()` — the function `lscp`/`mclp`/`p_center` called in
    `localalloc` without it ever being defined; implemented for real this
    time, shared by all four models, returns the `llocalocal_result` object
- `R/data.R` — `@docType data` documentation for the two bundled datasets
  below
- `R/print.R` (or folded into `utils.R`) — `print.llocalocal_result()`

## Data

Two bundled datasets, generated via `data-raw/` scripts (`usethis::use_data`
convention, scripts not part of the built package):

1. **Synthetic example** (`data-raw/sample-data.R`) — a small synthetic `sf`
   dataset (~10-20 demand points, ~10-20 candidates, a couple of existing
   sites) with a matching OD `data.frame`. Used in `@examples`, vignette,
   and test fixtures — small enough to solve instantly.
2. **Ported real dataset** (`data-raw/import-legacy-data.R`) — carries over
   `localalloc`'s `data/data.Rdata` as-is: `candidate_sites` (5,811 pts),
   `demand_pop` (176 pts), `existing_sites` (25 pts), `matrix_D_Candidates`
   (908,800 rows), `matrix_D_ExistingSites` (4,297 rows). Kept under its
   existing structure/column names; documented as a realistic large-scale
   example.

### Provenance (from `Essai_MarieHelene.pdf`, the source thesis)

This dataset is the case study from the thesis this package originates from:
BIXI bike-share station siting in the Fleurimont/Nations boroughs of
Sherbrooke, QC. `existing_sites` = the 25 real Bixi stations (BIXI open
data). `candidate_sites` = 5,811 candidate sites from OSM street segments
(cycleway/living_street/footway/residential), cut into 100 m lixels,
excluding slope > 5%. `demand_pop` = 176 dissemination-area (2021 census)
centroids, weight = population aged 15-64 at residence (open, via
`cancensus`) **plus** jobs at workplace. OD matrices = walking travel times
computed via `r5r` on an OSM+DEM network.

**Licensing flag, resolved:** the jobs-at-workplace figure folded into
`demand_pop$weight` comes from "a special compilation purchased from
Statistics Canada," not open data, and the two components are already
summed into one column (can't be separated post hoc). Raised with the user;
decision was to bundle the dataset as-is, on the user's own authority over
that data's usage terms — flagged here for traceability, not left as a
silent assumption. Carry a short provenance/licensing note in `R/data.R`'s
roxygen docs for `demand_pop` so downstream users see it too.

Suggested object names (clearer than the generic legacy names, and it's a
straight rename in the port script, not a design risk): `bixi_candidates`,
`bixi_demand`, `bixi_existing`, `bixi_od_candidates`, `bixi_od_existing`.

## Error handling

Every function validates through the shared `utils.R` helpers before
touching the solver: `sf` class/column checks, NA/duplicate id checks,
numeric-positivity checks on `cutoff_distance`/`p_facilities`/
`service_radius`, cost-matrix shape and non-negativity checks, and an
id-collision check between `candidate` and `existing_sites`. Errors and
warnings are in English. This is a straight port of `localalloc`'s
validation logic (which was solid), just centralized so all four functions
get the same guarantees instead of only `p_median`.

## Testing

- `testthat` edition 3
- One test file per exported function (`test-lscp.R`, `test-mclp.R`,
  `test-p_center.R`, `test-p_median.R`) plus `test-utils.R` for the shared
  helpers
- Fixtures built from the synthetic dataset; each model gets at least one
  case with a hand-checkable optimal solution (small enough to verify by
  inspection) plus coverage of the validation error paths
- `localalloc` shipped `testthat` scaffolding with an empty test file —
  this rewrite actually fills it in

## Guidelines followed

Standard `usethis`/`devtools`/`roxygen2`/`testthat` workflow per Wickham &
Bryan, *R Packages* (2e) — the same reference the source thesis cites for
package development practice.

## CI / tooling

- `usethis::use_github_action("check-standard")` — R-CMD-check on GitHub
  Actions (push/PR)
- `usethis::use_mit_license()`, `use_testthat(3)`, `use_readme_rmd()`,
  `use_news_md()`

## Environment

R 4.2.1 is installed at `C:\DEV\R\R-4.2.1` (not on PATH). None of the
required packages are installed yet: `sf`, `ompr`, `ompr.roi`, `ROI`,
`ROI.plugin.glpk`, `Matrix` (already present), `dplyr`, plus dev tooling
`devtools`, `usethis`, `testthat`, `roxygen2`, `knitr`, `rmarkdown`.
Installing these is the first implementation step — `devtools::document()`/
`test()`/`check()` can't run without them, and the plan must install and
verify them before any package code is written.

## Out of scope

- No CRAN submission — private package for now
- No GitHub repo creation/push automation — local git only
- No new models beyond the original four
- No vignette content beyond what's needed to demonstrate the two datasets
  (kept minimal; can grow later)
