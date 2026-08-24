# localloc — design

Date: 2026-07-29

## Summary

`localloc` is a clean rewrite and expansion of the `localloc` package:
all 10 facility-location ILP models from Table 2.1 of the source thesis
(`Essai_MarieHelene.pdf`) — P-Median, P-Center, MCLP, LSCP, UFCLP, CFLP, DP,
UFLP, MAXCAP, PMAXCAP — built on `ompr`/`ompr.roi`/`ROI.plugin.glpk`. It
replaces `localloc` in place in this repo.

The original 4 (LSCP, MCLP, P-Center, P-Median) reuse `localloc`'s solver
stack and model math as-is (that part was correct); the rewrite fixes
plumbing bugs (missing `.build_result()`, unexported functions, a
`poids_demand`/`weight_demand` typo, empty test file) and unifies all four
onto one input contract. The other 6 (UFCLP, CFLP, DP, UFLP, MAXCAP,
PMAXCAP) are new — `localloc` never implemented them, and no R or Python
library is cited in the thesis as covering them either (`spopt` only
implements the same 4; ArcGIS Network Analyst/TransCAD are proprietary and
don't map cleanly). These are fresh `ompr` formulations built from the
thesis's own math (§2.1.3–2.1.5).

## Package identity

- Name: `localloc`
- Author/maintainer: Philippe Apparicio <philippe.apparicio@usherbrooke.ca> (`aut`, `cre`)
- License: MIT + file LICENSE
- Language: English (roxygen docs, `stop()`/`warning()` messages)
- Location: this repo root, replacing the current `localloc` scaffold
  (`DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `Package.zip`, `localloc.Rproj`
  removed/rewritten). `Essai_MarieHelene.pdf` stays, added to
  `.Rbuildignore`.
- Version control: local git only (already initialized, baseline commit
  made). User creates the private GitHub repo and pushes; not done by
  Claude.

## Public API

The original four exported functions share one input contract (the other 6,
below, build on it with per-model deltas):

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

All ten return an S3 object of class `localloc_result` (fields:
`model_type`, `solver_status`, `sf_selected`, assignment table, and
model-appropriate stats such as `total_cost` or `max_distance`), printed via
one shared `print.localloc_result()`.

### The other 6 models (UFCLP, CFLP, DP, UFLP, MAXCAP, PMAXCAP)

Same solver stack throughout — all linear MIPs, including PMAXCAP (see
below), no nonlinear/MINLP solver dependency added.

| Function | Shape vs. the unified contract above | New inputs |
|---|---|---|
| `uflp()` | Identical to `p_median()` | none — `sense = "max"` internally (repels installations from demand instead of minimizing distance, per thesis eq. 2.26) |
| `ufclp()` | Drops `p_facilities` — facility count is endogenous, traded off against fixed cost | `candidate_fixed_cost` (column in `candidate`, f_j), `transport_cost_rate` (α, scalar) |
| `cflp()` | `ufclp()` + a capacity constraint per facility | `candidate_capacity` (column in `candidate`, k_j) |
| `dp()` | **Different shape** — no `demand`/`existing_sites` at all, purely site-to-site | `candidate` + `candidate_id` + `matrix_OD_candidates` (here meaning candidate-candidate distances) + `p_facilities` |
| `maxcap()` | Unified contract; `existing_sites` now required (= competitor sites) | `p_facilities` read as "at most p" (eq. 2.35 uses `<=`, not `=`) |
| `pmaxcap()` | `maxcap()` + pricing | `marginal_cost` (v), `distance_cost_rate` (t), `competitor_price` (P_B), `n_facilities` (n_A, "exactly n_A" per eq. 2.38), `candidate_fixed_cost` (optional, f_j, defaults to 0 — eq. 2.36 includes a fixed-cost term the first pass of this table missed), `max_breakpoints` (default 2000) |

**`uflp()`** — reverse p-median: same assignment/budget constraints (eqs.
2.27-2.29), objective flipped to maximize total weighted assigned distance,
pushing facilities away from demand.

**`ufclp()`/`cflp()`** — objective minimizes fixed + transport cost (eq.
2.16); `Y[i,j]` relaxed to continuous `[0,1]` per the thesis formulation
(eq. 2.20) rather than binary — still exact due to the total unimodularity
of the assignment substructure. `cflp()` adds
`sum_i a_i * Y[i,j] <= k_j * X[j]` per facility (eq. 2.21).

**`dp()`** — maximizes the minimum pairwise distance `D` between opened
sites (eq. 2.24), via the standard big-M linearization (eq. 2.25). Takes no
demand layer at all — it's a pure site-dispersion problem among
`candidate`s.

**`maxcap()`/`pmaxcap()` competition rule** — the thesis states `p_i`
(sites that would divert client i from its competitor) and `b_i^B` (client
i's current competitor choice) as given parameters without saying how to
derive them. This package derives them: `b_i^B` = each demand point's
nearest `existing_sites` facility by OD distance; `p_i`/tie-set `T_i` =
candidates strictly closer (or, for `pmaxcap()`, closer net of price) than
that baseline.

**`pmaxcap()` algorithm** — for a fixed site selection, profit is
piecewise-linear in price `P_A` (capture only flips at threshold
breakpoints). The exact global optimum is found by enumerating every
breakpoint `P_B + t·d_i,b_i^B − t·d_ij` (one per demand×candidate pair),
solving one linear MIP per breakpoint (same shape as `maxcap()` at that
fixed price), and keeping the best. This is exact — not an approximation —
and needs no nonlinear/MINLP solver, which is good because none is
installed or a natural fit here. `max_breakpoints` (default 2000) caps the
enumeration: under the cap, exact; over it (e.g. the ~1M breakpoints at the
real Sherbrooke dataset's scale), warns and subsamples rather than hanging.
Documented as a real limitation, not hidden — this is inherent to solving
PMAXCAP exactly with a linear-only solver stack at that scale, not a
shortcut taken for convenience.

## Internal architecture

- `R/lscp.R`, `R/mclp.R`, `R/p_center.R`, `R/p_median.R`, `R/uflp.R`,
  `R/ufclp.R`, `R/cflp.R`, `R/dp.R`, `R/maxcap.R`, `R/pmaxcap.R` — one
  exported function each. Each is: validate inputs → build the merged
  candidate+existing cost matrix via shared helpers → build the `ompr`
  model (binary `X[j]`, and for assignment models `Y[i,j]`) → solve via
  `ompr.roi::with_ROI(solver)` → hand off to a shared `build_result()`.
  `dp()` skips the demand/cost-matrix-merge step (site-to-site only).
  `pmaxcap()` wraps the per-breakpoint solve loop around the same
  build-model-solve shape as `maxcap()`.
- `R/utils.R` — internal (`.`-prefixed or `@noRd`) helpers used across all
  ten:
  - `validate_sf()` / `validate_cost_matrix()` — shape/NA/duplicate-id
    checks, applied identically across all ten models (`localloc` only
    gave full validation to `p_median` — this is the actual bug fix, not
    just a rename)
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
    `localloc` without it ever being defined; implemented for real this
    time, shared by all ten models, returns the `localloc_result` object
  - `validate_fixed_cost()` / `validate_capacity()` — column
    presence/non-negativity checks for `ufclp()`/`cflp()`
  - `derive_competitor_baseline()` — computes `b_i^B`, `p_i`, `T_i` for
    `maxcap()`/`pmaxcap()`
  - `enumerate_breakpoints()` — breakpoint construction + `max_breakpoints`
    subsampling for `pmaxcap()`
- `R/data.R` — `@docType data` documentation for the two bundled datasets
  below
- `R/print.R` (or folded into `utils.R`) — `print.localloc_result()`

## Data

Two bundled datasets, generated via `data-raw/` scripts (`usethis::use_data`
convention, scripts not part of the built package):

1. **Synthetic example** (`data-raw/sample-data.R`) — a small synthetic `sf`
   dataset (~10-20 demand points, ~10-20 candidates, a couple of existing
   sites) with a matching OD `data.frame`. Used in `@examples`, vignette,
   and test fixtures — small enough to solve instantly.
2. **Ported real dataset** (`data-raw/import-legacy-data.R`) — carries over
   `localloc`'s `data/data.Rdata` as-is: `candidate_sites` (5,811 pts),
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
warnings are in English. This is a straight port of `localloc`'s
validation logic (which was solid), just centralized and extended so all
ten functions get the same guarantees instead of only `p_median`.

## Testing

- `testthat` edition 3
- One test file per exported function — the original 4 (`test-lscp.R`,
  `test-mclp.R`, `test-p_center.R`, `test-p_median.R`) plus 6 more
  (`test-uflp.R`, `test-ufclp.R`, `test-cflp.R`, `test-dp.R`,
  `test-maxcap.R`, `test-pmaxcap.R`), plus `test-utils.R` for the shared
  helpers
- Fixtures built from the synthetic dataset; each model gets at least one
  case with a hand-checkable optimal solution (small enough to verify by
  inspection) plus coverage of the validation error paths
- `test-pmaxcap.R`'s fixture is sized so `max_breakpoints` never triggers —
  the test verifies the exact breakpoint-enumeration algorithm, not the
  subsampling fallback
- `localloc` shipped `testthat` scaffolding with an empty test file —
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
- No models beyond the 10 in Table 2.1 of the source thesis
- `pmaxcap()`'s exactness is capped by `max_breakpoints` — full-scale
  Sherbrooke-size exact solves are a known, documented limitation, not
  something this plan fixes (would need a smarter breakpoint-pruning
  algorithm, left for later if it's ever needed in practice)
- No vignette content beyond what's needed to demonstrate the two datasets
  (kept minimal; can grow later)
