# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`localalloc` — an R package implementing facility location optimization models
(LSCP, MCLP, P-Median, P-Center) as integer linear programs, solved via `ompr`
+ `ROI`/`ROI.plugin.glpk`. Author: Marie-Hélène Gadbois-Del Carpio (USherbrooke).

The repo root *is* the package root (DESCRIPTION, NAMESPACE, R/, man/, tests/
at top level). `Package.zip` in the root is the original delivery archive this
was unpacked from — the unpacked source is now what's tracked; the zip is
redundant and can be deleted once you've confirmed nothing else depends on it.

## Common commands

Run from an R console with `devtools` installed, working directory = repo root:

```r
devtools::load_all()      # load package for interactive dev
devtools::document()      # regenerate NAMESPACE/man/*.Rd from roxygen comments (roxygen2, markdown=TRUE)
devtools::test()          # run testthat suite
devtools::check()         # full R CMD check
```

Single test file: `testthat::test_file("tests/testthat/test-lscp.R")`.

There is no build/test tooling outside of R itself (no Makefile, no CI config).

## Architecture

Four model functions, one shared helper file:

- `R/lscp.R` — `lscp()`: minimum facilities to cover all demand within a radius.
- `R/mclp.R` — `mclp()`: maximize demand covered with a fixed facility budget.
- `R/p_center.R` — `p_center()`: minimize the worst-case (max) client-facility distance.
- `R/p_median.R` — `p_median()`: minimize total weighted distance; the most
  developed of the four (see below).
- `R/utilitaires.R` — internal (`.`-prefixed) helpers shared by all four models,
  plus `print.localalloc_result`.

All four follow the same shape: validate inputs → build an `ompr::MIPModel()`
with binary `X[j]` (facility open) and, for assignment models, `Y[i,j]`
(client-facility assignment) variables → solve with
`ompr.roi::with_ROI(solver = solver)` (default `"glpk"`) → extract the
solution → return `structure(list(...), class = "localalloc_result")`.

**Export status matters here**: NAMESPACE currently exports only `p_median`
(plus the `print.localalloc_result` S3 method). `lscp`, `mclp`, and `p_center`
have no `@export` tag and aren't in NAMESPACE — they're not part of the public
API yet. They also call `.build_result()`, which is not defined anywhere in
`R/utilitaires.R` (only `.build_result_sf()` is) — these three are unfinished/
non-functional as written. Don't assume they work; check before building on
them, and run `devtools::document()` after adding `@export` tags if you wire
one up.

**Two input conventions coexist**:
- `lscp`, `mclp`, `p_center` take a precomputed numeric `cost_matrix` (clients
  × facilities) directly.
- `p_median` takes raw `sf` POINT layers (`candidate`, `demand`, optional
  `existing_sites`) plus long-format origin-destination `data.frame`s
  (`matrix_OD_candidates`, optional `matrix_OD_existing_site`), and builds the
  cost matrix itself via `.od_to_matrix()`. It also supports "Required
  Facilities": `existing_sites` are forced open (`X[j] == 1`) in the ILP
  rather than competing for the budget.

Key shared helpers in `utilitaires.R` (all internal, no roxygen `@export`):
- `.validate_sf()` / `.validate_cost_matrix()` — input validation, French-language `stop()`/`warning()` messages (the whole package's user-facing messages are in French).
- `.od_to_matrix()` — long OD `data.frame` → wide matrix, `Inf` for missing/over-cutoff pairs.
- `.replace_inf()` — swaps `Inf` for a large finite value (solver compatibility).
- `.make_coverage_matrix()` — distance matrix → binary coverage matrix for LSCP/MCLP.
- `.set_weights()` — fills missing weight columns on `sf` objects with 1.
- `.extract_assignment()` / `.build_result_sf()` — turn `ompr::get_solution()` output back into `data.frame`/`sf` results.

`tests/testthat/test-lscp.R` exists but is empty — there is no real test
coverage yet despite the `testthat` scaffolding being in place.

`data/data.Rdata` holds example/sample data for the package (not inspected in
detail — check its contents with `load()` before relying on its structure).
