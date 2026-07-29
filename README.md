# llocalocal

Ten facility-location optimization models (P-Median, P-Center, MCLP,
LSCP, UFCLP, CFLP, DP, UFLP, MAXCAP, PMAXCAP) as integer linear
programs, solved via `ompr` and `ROI.plugin.glpk`.

## Installation

```r
# not on CRAN -- install from the local clone or the private GitHub repo
devtools::install_local(".")
```

## Example

```r
suppressPackageStartupMessages(library(llocalocal))

res <- p_median(
  demand = sample_demand, demand_id = "id",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  p_facilities = 3
)
res
```
```
#> P_MEDIAN | 15 demand points | 12 candidates (p=3) | solver: glpk
#>
#> =======================================================
#>   Model         : P_MEDIAN
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 3
#>   Total cost       : 36.96
#> =======================================================
#>
```

See `?bixi-data` for the real-world Sherbrooke Bixi bike-share dataset
this package originates from.
