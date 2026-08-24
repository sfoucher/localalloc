# localalloc — Implementation Report

Date: 2026-07-30
Branch merged: `worktree-localalloc-implementation` → `main` (fast-forward, commit `fd5b057`)
Pushed to: `sfoucher/localalloc` (private)

## What was built

`localalloc`, a from-scratch rewrite/expansion of the prior `localalloc` package: 10 facility-location optimization models as integer linear programs, solved via `ompr`/`ROI.plugin.glpk`.

- **Models:** `p_median`, `p_center`, `mclp`, `lscp`, `ufclp`, `cflp`, `dp`, `uflp`, `maxcap`, `pmaxcap`
- **Shared internals:** `R/utils.R` (validation, matrix-building, result construction), `.assignment_model()` (shared by `p_median`/`uflp`), `.fixed_charge_model()` (shared by `ufclp`/`cflp`)
- **Data:** a small synthetic example (`sample_*`) and the real Sherbrooke, QC Bixi bike-share dataset (`bixi_*`, 5,811 candidates / 176 demand points / 25 existing stations), ported from the old package
- **Packaging:** MIT license, testthat (ed. 3, 66 assertions), roxygen2 docs, README/NEWS, GitHub Actions CI

## Process

Spec → 18-task implementation plan → subagent-driven execution (one implementer + one task-scoped reviewer per task, fix loops as needed) → two independent final whole-branch reviews → one consolidated fix wave → scoped re-review → merge.

## Bugs found and fixed during the process

These were real defects caught by review, not hypothetical — each is confirmed and fixed in the merged code.

1. **`mclp()` accepted `existing_sites` but silently ignored it** (Task 5 review). Fixed: implemented Required-Facilities semantics matching the other models.
2. **`result$status != "optimal"` never true in this environment** — glpk actually returns `"success"`, so every model fired a spurious "non-optimal" warning on every successful solve (Task 6 review, caught by chance while reviewing `p_center`). Fixed everywhere before it could propagate into the remaining models.
3. **Scalar `ompr` variables (`Z`, `D`) extracted with `$value`**, which errors on non-indexed variables (`ompr::get_solution()` returns a plain atomic vector for those, not a data.frame). Caught independently in both `p_center` (Task 6) and `dp` (Task 11); fixed with `as.numeric(...)`.
4. **`DESCRIPTION` missing `LazyData: true`** — bundled datasets weren't accessible as bare object names without it (Task 14).
5. **`rmarkdown::render()` fundamentally broken in this environment** — frozen CRAN binary snapshot for R 4.2 has `xfun` 0.43, incompatible with installed `knitr`/`rmarkdown`, and no Rtools to compile a newer one. Worked around by hand-producing `README.md` from genuine captured output (verified byte-for-byte against a live run, twice, by two reviewers) rather than attempting the render (Task 16).
6. **`ROI.plugin.glpk` declared in `Imports` but never referenced in code** — its solver-registration `.onLoad()` never ran under plain `library(localalloc)`, only appearing to work throughout the whole build because `pkgload::load_all()`'s dev-mode loads all Imports regardless of use. Every model would have failed for a real installed-package user with the default solver. Found during the Task 18 `R CMD check` pass; fixed via `@import ROI.plugin.glpk`; independently verified with a real `R CMD INSTALL` + fresh session (not just `load_all()`) by two separate reviewers.
7. **`sf` demoted to `Suggests` to silence the same "unused import" NOTE** — this took the wrong branch of the same fix as #6: it left a latent bug where `[.sf` subsetting silently degrades to `[.data.frame` (corrupting the geometry column) if `sf`'s namespace never loaded, reachable via the package's own bundled `LazyData` sf datasets without a user ever calling `library(sf)`. Fixed by keeping `sf` in `Imports` with `@importFrom sf st_geometry` instead.
8. **`print.localalloc_result()` under-reported facility count** whenever `existing_sites` was used — it read `nrow(x$sf_selected)` (candidates only) instead of `x$n_open` (which correctly includes forced-open existing sites). Caught only by the whole-branch review, since no single task's tests happened to `print()` a result built with `existing_sites`.
9. **`od_to_matrix()` crashed with "subscript out of bounds"** whenever a candidate/demand id had zero rows in its OD table — confirmed as a real, live risk: 2 of the bundled `bixi_candidates`' 5,811 ids have zero rows in `bixi_od_candidates`. Fixed by having `od_to_matrix()` build the full requested id universe up front (missing ids become `Inf` rows/columns instead of absent dimensions) across all 8 model files / 14 call sites.
10. **`bixi_existing`'s 25 ids are a complete subset of `bixi_candidates`'s ids** — every Required-Facilities model's collision guard (`candidate`/`existing_sites` id overlap) hard-errored unconditionally on the bundled real dataset, meaning the flagship dataset could never demonstrate the package's own headline feature. Fixed (per your decision) by softening the guard in `lscp`/`mclp`/`p_center`/`p_median` from a hard `stop()` to auto-excluding the overlapping ids from `candidate` with a `warning()` — `maxcap`/`pmaxcap` deliberately left as hard errors, since `existing_sites` means "the competitor" there, a genuine data contradiction if it overlaps.
11. **`demand_weight` roxygen docs wrong on 5 of 10 models** — inherited LSCP's "unused" wording via `@inheritParams` chains, factually false for models where the weight actually drives the objective (`p_median`, `uflp`, `maxcap`, `pmaxcap`), and even LSCP's own doc overclaimed "validated for consistency" when it never validated anything. Fixed with model-accurate text in 4 files (2 more corrected automatically via inheritance).
12. Two prior plan-text bugs caught and fixed before they shipped: a wrong expected value in a `derive_competitor_baseline` test (column-major matrix-fill arithmetic error), and a missing `matrix_OD_existing_site_from_id`/`_to_id`/`_dist` argument in a test exercising the optional-existing_sites path.

## Known limitations (deferred, not blocking)

- **`candidate_weight`/`existing_sites_weight` are accepted parameters on all 10 models but never used anywhere.** Interface-symmetry placeholders from the unified design contract; currently inert dead weight in every function signature. Worth either documenting explicitly as reserved-for-future-use or removing.
- **`p_center()` has no `cutoff_distance` validation**, unlike sibling `lscp()`/`mclp()`. A bad value degrades results (wrong pruning) rather than erroring clearly. Low severity, one-line fix whenever someone's next in that file.
- **`pmaxcap()` scaling gaps**, all pre-acknowledged by the design spec's own scaling discussion: recomputes an invariant threshold matrix every breakpoint-loop iteration (wasted work at the ~1M-breakpoint real-dataset scale); a solver error inside the loop aborts the whole enumeration instead of skip-and-warn; `max_breakpoints` isn't coerced to integer.
- **`R/data.R` claims the sample dataset is used "in the vignette,"** but there is no `vignettes/` directory despite `DESCRIPTION` declaring `VignetteBuilder: knitr`. Dangling metadata, zero functional impact — either write a minimal vignette (the Bixi dataset would make a good one) or remove the claim/`VignetteBuilder` field.
- **`p_center()` duplicates `.assignment_model()`'s ~90 lines of validation/matrix logic** rather than sharing it — a sequencing artifact (p_center predates the shared helper in the build order), not an oversight. Any future fix to shared validation logic needs to land in both places until refactored.
- **`bixi_demand`'s `weight` column bakes in a Statistics-Canada special-compilation figure** (purchased, not open data) already summed with open census population — documented verbatim in the roxygen docs per your earlier decision to bundle it on your own authority over that data's usage terms. Worth re-confirming before any wider redistribution of the package.
- **This package is not intended for CRAN submission** (private repo, per the original design spec) — the `rcmdcheck` pass targeted "0 errors/warnings/notes," not CRAN policy compliance beyond that.
- **No R or Python library was ever cited as covering the 6 non-original models** (UFCLP, CFLP, DP, UFLP, MAXCAP, PMAXCAP) — these are fresh `ompr` formulations built directly from the source thesis's math, not ports of an existing, battle-tested implementation. `pmaxcap()`'s exact breakpoint-enumeration algorithm in particular was independently hand-verified by a reviewer against the actual fixture arithmetic, but it's inherently the newest, least-precedented piece of the package.

## Environment constraints hit during the build

- `devtools`/`usethis` are unavailable in this R 4.2.1 environment (CRAN's Windows binary snapshot for R 4.2 is frozen, no Rtools to compile their `httr2`/`gh` dependency chain). Routed around via `pkgload`, `roxygen2`, `testthat`, `rcmdcheck`, and hand-written files wherever `usethis` would normally scaffold them (LICENSE, `.github/workflows/*.yaml`, README.Rmd, NEWS.md).
- Same root cause blocked `rmarkdown::render()` outright (see fix #5 above).

## Verification evidence

- Full test suite: 66/66 assertions passing, 0 failures/warnings, on the merged `main` branch (re-verified after merge, not just pre-merge).
- `rcmdcheck::rcmdcheck()`: 0 errors, 0 warnings, 0 notes.
- Two independent whole-branch reviews (one on Sonnet after Opus hit a weekly rate limit) converged on the same core findings; the most consequential claims (ROI.plugin.glpk fix, `od_to_matrix` crash) were verified via live `R CMD INSTALL` + fresh sessions, not just static code reading.

## CI status

GitHub Actions (`R-CMD-check.yaml`) was triggered by the push to `main` and was still running (in progress, multi-OS matrix) as of this report — not yet confirmed green on GitHub's runners specifically. Worth checking `gh run list --repo sfoucher/localalloc` once it completes.
