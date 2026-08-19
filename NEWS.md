# localloc 0.1.0

* Initial release. Ten facility-location models: `p_median()`,
  `p_center()`, `mclp()`, `lscp()`, `ufclp()`, `cflp()`, `dp()`,
  `uflp()`, `maxcap()`, `pmaxcap()`.
* Rewrite of the original `localloc` code base: fixes an undefined
  `.build_result()` call, missing exports, and an unused test file;
  unifies all model functions onto one sf/OD input contract.
* Ships two datasets: a small synthetic example (`sample_*`) and the
  real Sherbrooke Bixi bike-share case study (`bixi_*`) from the
  source thesis.
