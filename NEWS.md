# localalloc 0.2.0

* Solver engine replaced: dropped `ompr`/`ROI` in favor of building each
  model's MIP directly as a sparse matrix (`Matrix::sparseMatrix()`) and
  solving via `Rglpk` or `highs` (new default). Large instances that took
  minutes under `ompr` now solve in seconds.
* `existing_sites` semantics changed for `mclp()`/`p_median()`/`p_center()`/
  `uflp()`: forced-open sites now count *inside* the `p_facilities` budget
  (`p_facilities` is the total number of open facilities, not just new
  ones); `sf_selected` returns every open site with a `source` column
  (`"candidate"`/`"existing"`). `maxcap()`/`pmaxcap()` are unchanged
  (`existing_sites` there is the competitor, not ours).
* Added `plot_sites()`: interactive `mapview` map of candidate/demand/
  existing sf layers.
* Package renamed several times since 0.1.0: `localalloc` -> `localocal`
  -> `localloc` -> `localalloc`.
* README rewritten with a description and a runnable example for all ten
  models.

# localalloc 0.1.0

* Initial release. Ten facility-location models: `p_median()`,
  `p_center()`, `mclp()`, `lscp()`, `ufclp()`, `cflp()`, `dp()`,
  `uflp()`, `maxcap()`, `pmaxcap()`.
* Rewrite of the original `localalloc` code base: fixes an undefined
  `.build_result()` call, missing exports, and an unused test file;
  unifies all model functions onto one sf/OD input contract.
* Ships two datasets: a small synthetic example (`sample_*`) and the
  real Sherbrooke Bixi bike-share case study (`bixi_*`) from the
  source thesis.
