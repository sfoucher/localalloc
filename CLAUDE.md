# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`llocalocal` — an R package implementing ten facility-location optimization
models (LSCP, MCLP, P-Median, P-Center, UFCLP, CFLP, DP, UFLP, MAXCAP,
PMAXCAP) as integer linear programs, solved via `ompr` +
`ROI`/`ROI.plugin.glpk`. Author: Philippe Apparicio (USherbrooke). All ten
are exported; result objects carry class `llocalocal_result`.

The repo root *is* the package root (DESCRIPTION, NAMESPACE, R/, man/, tests/
at top level). `data-raw/` holds the scripts that generated `data/*.rda`
(`import-legacy-data.R` for the real `bixi_*` case study, `sample-data.R`
for the small `sample_*` fixtures).

## Common commands

Run from an R console with `devtools` installed, working directory = repo root:

```r
devtools::load_all()      # load package for interactive dev
devtools::document()      # regenerate NAMESPACE/man/*.Rd from roxygen comments (roxygen2, markdown=TRUE)
devtools::test()          # run testthat suite
devtools::check()         # full R CMD check
```

Single test file: `testthat::test_file("tests/testthat/test-lscp.R")`.

CI runs R CMD check on push/PR via `.github/workflows/R-CMD-check.yaml`
(r-lib/actions). No Makefile.

## Architecture

Ten model functions (`R/lscp.R`, `mclp.R`, `p_center.R`, `p_median.R`,
`ufclp.R`, `cflp.R`, `dp.R`, `uflp.R`, `maxcap.R`, `pmaxcap.R`), one shared
internal helper file `R/utils.R` (validation, matrix building, result
extraction, English-language `stop()`/`warning()` messages), plus `R/print.R`
(`print.llocalocal_result`) and `R/visualize.R` (`plot_sites()` — interactive
`mapview` map of candidate/demand/existing sf layers; `mapview` is a
`Suggests`, guarded by `requireNamespace()`).

All models follow the same shape: validate inputs → build an `ompr::MIPModel()`
with binary `X[j]` (facility open) and, for assignment models, `Y[i,j]`
(client-facility assignment) variables → solve with
`ompr.roi::with_ROI(solver = solver)` (default `"glpk"`) → extract the
solution → return `structure(list(...), class = "llocalocal_result")`. Check
the actual function/NAMESPACE before assuming a helper name — this file has
drifted from the code before and will again.

All 10 models share one input contract: raw `sf` POINT layers
(`candidate`, `demand`, optional `existing_sites`) plus long-format
origin-destination `data.frame`s (`matrix_OD_candidates`, optional
`matrix_OD_existing_site`), turned into a cost matrix via
`od_to_matrix()`. Models that support `existing_sites` force them open
("Required Facilities", `X[j] == 1`) rather than competing for the
budget — except `maxcap`/`pmaxcap`, where `existing_sites` is the
*competitor* instead. `dp()` is the odd one out: no demand layer at
all, just a candidate-to-candidate distance table.

Key shared helpers in `utils.R` (all internal, no roxygen `@export`):
- `validate_sf()` / `validate_cost_matrix()` — input validation, English `stop()`/`warning()` messages.
- `od_to_matrix()` — long OD `data.frame` → wide matrix, `Inf` for missing/over-cutoff pairs.
- `replace_inf()` — swaps `Inf` for a large finite value (solver compatibility).
- `make_coverage_matrix()` — distance matrix → binary coverage matrix for LSCP/MCLP.
- `set_weights()` — when `weight_col` is NULL, *overwrites* the `weight`
  column with 1 (even if one already has real data) rather than only
  filling it if absent. Intentional "unweighted by default" design, but
  looks like a bug if you forget to pass `demand_weight = "weight"` and
  wonder why a capacity/weighting constraint isn't binding.
- `extract_assignment()` / `build_result_sf()` — turn `ompr::get_solution()` output back into `data.frame`/`sf` results.

Full test coverage exists — one `test-<model>.R` per model plus
`test-utils.R` and `helper-fixtures.R`.

`data/bixi_*.rda` — real Sherbrooke Bixi case study (5,811 candidates × 176
demand pts × 25 existing stations; OD tables use `from_id`/`to_id`/
`travel_time_p50` columns). Running an assignment model (e.g. `p_median()`)
at full bixi scale builds ~1M binary `Y[i,j]` vars via `ompr` and OOM-crashes
silently (no R error, ~4 min in, ~16GB RAM box) — subsample `candidate`
(and the matching OD rows) before timing/testing at this scale.
`data/sample_*.rda` — small fixtures for quick manual runs.
