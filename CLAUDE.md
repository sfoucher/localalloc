# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`llocalocal` — an R package implementing ten facility-location optimization
models (LSCP, MCLP, P-Median, P-Center, UFCLP, CFLP, DP, UFLP, MAXCAP,
PMAXCAP) as integer linear programs, built as sparse matrices and solved
directly via `Rglpk` or `highs` (default). Author: Philippe Apparicio
(USherbrooke). All ten are exported; result objects carry class
`llocalocal_result`.

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

All ten models follow the same shape: validate inputs → build a cost matrix
via `od_to_matrix()` → derive `valid <- which(is.finite(cost_mat), arr.ind = TRUE)`
(only in-cutoff/in-radius pairs get a decision variable at all, keeping the
MIP sparse) → build the objective/constraints directly as a
`Matrix::sparseMatrix()` (never a dense `matrix`, never a symbolic DSL) →
dispatch to `solve_direct(L, A, dir, rhs, types, lower, upper, sense, solver)`
in `utils.R` → extract the solution → return
`structure(list(...), class = "llocalocal_result")`. Check the actual
function/NAMESPACE before assuming a helper name — this file has drifted
from the code before and will again.

**Solver engine — no `ompr`/`ROI`.** Earlier versions built the MIP with
`ompr::MIPModel()` and solved via `ompr.roi::with_ROI()`. That was dropped:
`ompr`'s own objective-vector construction
(`objective_function.linear_optimization_model`) assigns coefficients into a
`Matrix::sparseVector` one element at a time, and each assignment re-sorts
the whole structure — cheap individually, catastrophic in aggregate on
problems with tens of thousands of terms (minutes, regardless of which
solver `ompr` was asked to use). `solve_direct()` bypasses this entirely:
each model builds its own sparse `A`/`L`/`dir`/`rhs`/`types` once, and
`solve_direct()` hands them straight to `Rglpk::Rglpk_solve_LP()` (solver
`"glpk"`) or `highs::highs_solve()` (`"highs"`, the default — genuinely
faster than glpk now that the `ompr` bottleneck is gone, confirmed on the
`bixi_*` case study). `types = "B"` works natively for glpk; for highs it's
translated to `"I"` + bounds `[0,1]` (the `highs` package has no binary
type). Variable order within each model's vector is whatever that model's
comment block documents (typically `[Y, X]` or `[Y, X, Z]`) — read the top
of the sparse-matrix block before touching indices.

All 10 models still share one input contract: raw `sf` POINT layers
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
- `replace_inf()` — swaps `Inf` for a large finite value; still used by `dp()` (missing-pair fallback, no cutoff concept there) and by `maxcap()`/`pmaxcap()` (as a comparison sentinel for their competitor-baseline `bij` matrix) — NOT used by the assignment-style models anymore, since those now only create a decision variable for pairs that are already finite.
- `make_coverage_matrix()` — distance matrix → binary coverage matrix for LSCP/MCLP.
- `set_weights()` — when `weight_col` is NULL, *overwrites* the `weight`
  column with 1 (even if one already has real data) rather than only
  filling it if absent. Intentional "unweighted by default" design, but
  looks like a bug if you forget to pass `demand_weight = "weight"` and
  wonder why a capacity/weighting constraint isn't binding.
- `extract_assignment()` — turns a `data.frame(i, j, value)` solution back into a demand→facility assignment `data.frame`; solver-agnostic, unchanged by the `ompr` removal.
- `build_result()` — assembles the final `llocalocal_result` list.
- `solve_direct()` — the glpk/highs dispatcher described above.

Full test coverage exists — one `test-<model>.R` per model plus
`test-utils.R` and `helper-fixtures.R`.

`data/bixi_*.rda` — real Sherbrooke Bixi case study (5,811 candidates × 176
demand pts × 25 existing stations; OD tables use `from_id`/`to_id`/
`travel_time_p50` columns). A 400-candidate subsample of `p_median()` solves
in ~3s with the current (direct sparse-matrix) engine — previously 220s+
under the old `ompr`-based engine. Full-scale (5,811 candidates) has not
been re-benchmarked since the rewrite; the old OOM warning no longer applies
as-is (no more `ompr`-side dense variable blowup), but re-test before
assuming it's fine at full scale. `data/sample_*.rda` — small fixtures for
quick manual runs.
