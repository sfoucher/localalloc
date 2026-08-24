# Changelog

## localalloc 0.2.0

- Solver engine replaced: dropped `ompr`/`ROI` in favor of building each
  model’s MIP directly as a sparse matrix
  ([`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html))
  and solving via `Rglpk` or `highs` (new default). Large instances that
  took minutes under `ompr` now solve in seconds.
- `existing_sites` semantics changed for
  [`mclp()`](https://sfoucher.github.io/localalloc/reference/mclp.md)/[`p_median()`](https://sfoucher.github.io/localalloc/reference/p_median.md)/[`p_center()`](https://sfoucher.github.io/localalloc/reference/p_center.md)/
  [`uflp()`](https://sfoucher.github.io/localalloc/reference/uflp.md):
  forced-open sites now count *inside* the `p_facilities` budget
  (`p_facilities` is the total number of open facilities, not just new
  ones); `sf_selected` returns every open site with a `source` column
  (`"candidate"`/`"existing"`).
  [`maxcap()`](https://sfoucher.github.io/localalloc/reference/maxcap.md)/[`pmaxcap()`](https://sfoucher.github.io/localalloc/reference/pmaxcap.md)
  are unchanged (`existing_sites` there is the competitor, not ours).
- Added
  [`plot_sites()`](https://sfoucher.github.io/localalloc/reference/plot_sites.md):
  interactive `mapview` map of candidate/demand/ existing sf layers.
- Package renamed several times since 0.1.0: `localalloc` -\>
  `localocal` -\> `localloc` -\> `localalloc`.
- README rewritten with a description and a runnable example for all ten
  models.

## localalloc 0.1.0

- Initial release. Ten facility-location models:
  [`p_median()`](https://sfoucher.github.io/localalloc/reference/p_median.md),
  [`p_center()`](https://sfoucher.github.io/localalloc/reference/p_center.md),
  [`mclp()`](https://sfoucher.github.io/localalloc/reference/mclp.md),
  [`lscp()`](https://sfoucher.github.io/localalloc/reference/lscp.md),
  [`ufclp()`](https://sfoucher.github.io/localalloc/reference/ufclp.md),
  [`cflp()`](https://sfoucher.github.io/localalloc/reference/cflp.md),
  [`dp()`](https://sfoucher.github.io/localalloc/reference/dp.md),
  [`uflp()`](https://sfoucher.github.io/localalloc/reference/uflp.md),
  [`maxcap()`](https://sfoucher.github.io/localalloc/reference/maxcap.md),
  [`pmaxcap()`](https://sfoucher.github.io/localalloc/reference/pmaxcap.md).
- Rewrite of the original `localalloc` code base: fixes an undefined
  `.build_result()` call, missing exports, and an unused test file;
  unifies all model functions onto one sf/OD input contract.
- Ships two datasets: a small synthetic example (`sample_*`) and the
  real Sherbrooke Bixi bike-share case study (`bixi_*`) from the source
  thesis.
