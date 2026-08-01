
# llocalocal

Ten facility-location optimization models (P-Median, P-Center, MCLP,
LSCP, UFCLP, CFLP, DP, UFLP, MAXCAP, PMAXCAP) as integer linear
programs, solved via `ompr` and `ROI.plugin.glpk`. The models follow the
theoretical framework in `Essai_MarieHelene.pdf`, the thesis this
package originates from.

## Installation

``` r
# not on CRAN -- install from the local clone or the private GitHub repo
devtools::install_local(".")
```

## Models

Every model shares the same input contract: `demand`/`candidate`/
`existing_sites` are `sf` POINT layers with an id column, and
`matrix_OD_*` are long origin-destination `data.frame`s
(`from_id`/`to_id`/`distance`) linking them. The examples below all use
the small bundled synthetic dataset (`sample_*`: 15 demand points, 12
candidates, 3 existing sites) so they solve instantly, except MCLP,
which is demonstrated on the real Sherbrooke Bixi dataset bundled with
the package.

### P-Median

Selects exactly `p_facilities` candidate sites to minimize the total
distance between each demand point and its nearest open facility,
weighted by demand (population, jobs, etc.). Sites in `existing_sites`,
if supplied, are forced open (“Required Facilities”) instead of
competing for the budget.

``` r
suppressPackageStartupMessages(library(llocalocal))

res <- p_median(
  demand = sample_demand, demand_id = "id",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  p_facilities = 3
)
#> P_MEDIAN | 15 demand points | 12 candidates (p=3) | solver: glpk
res
#> 
#> =======================================================
#>   Model         : P_MEDIAN
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 3
#>   Total cost       : 36.96
#> =======================================================
```

Version with a subsampled bixi dataset:

``` r
devtools::load_all(".", quiet = TRUE)
data(bixi_candidates); data(bixi_demand)
data(bixi_od_candidates)

set.seed(1)
sub_ids <- bixi_candidates$id[seq(1, nrow(bixi_candidates), by = 10)]
cand_sub <- bixi_candidates[bixi_candidates$id %in% sub_ids, ]
od_sub <- bixi_od_candidates[bixi_od_candidates$to_id %in% sub_ids, ]

cat(sprintf("candidates=%d demand=%d od_rows=%d\n",
            nrow(cand_sub), nrow(bixi_demand), nrow(od_sub)))

t0 <- Sys.time()
res <- p_median(
  demand = bixi_demand, demand_id = "id",
  candidate = cand_sub, candidate_id = "id",
  matrix_OD_candidates = od_sub,
  matrix_OD_candidates_from_id = "from_id",
  matrix_OD_candidates_to_id = "to_id",
  matrix_OD_candidates_dist = "travel_time_p50",
  cutoff_distance = 1000,
  p_facilities = 5,
  solver = "glpk"
)
t1 <- Sys.time()
elapsed <- difftime(t1, t0)
cat(sprintf("ELAPSED: %.2f %s\n", as.numeric(elapsed), units(elapsed)))
cat("n_open:", res$n_open, " total_cost:", res$total_cost, "\n")
print(res$sf_selected$id)
# p_median on subsampled bixi (582 candidates × 176 demand, p=5, glpk): 13.92 min. 5 sites opened, total cost 3913.
```

### P-Center

Shares P-Median’s setup and constraints, but minimizes the *maximum*
(worst-case) distance between any demand point and its assigned facility
instead of the weighted total – a minimax rather than a minisum
objective.

``` r
res <- p_center(
  demand = sample_demand, demand_id = "id",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  p_facilities = 3
)
#> P_CENTER | 15 demand points | 12 candidates (p=3) | solver: glpk
res
#> 
#> =======================================================
#>   Model         : P_CENTER
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 3
#>   Max distance     : 4.06
#> =======================================================
```

### MCLP

Selects up to `p_facilities` candidate sites to maximize the weighted
demand covered within `service_radius`. Unlike LSCP, full coverage isn’t
required – demand outside every open facility’s radius is simply left
uncovered. `existing_sites`, if supplied, count as already-open coverage
and don’t consume the facility budget.

The package also bundles the real-world case study it originates from:
5,811 candidate sites, 176 demand points, and 25 existing Bixi
bike-share stations in Sherbrooke, QC (see `?bixi-data` for full
provenance and a data-licensing note). `existing_sites` are treated as
Required Facilities – forced open, and excluded automatically (with a
warning) from `candidate` where the two overlap.

This is a real ~5,800-variable MIP, not a toy example – expect it to
take a few minutes to solve with the default `glpk` solver.

``` r
res <- mclp(
  demand = bixi_demand, demand_id = "id", demand_weight = "weight",
  candidate = bixi_candidates, candidate_id = "id",
  existing_sites = bixi_existing, existing_sites_id = "id",
  matrix_OD_candidates = bixi_od_candidates,
  matrix_OD_candidates_from_id = "from_id",
  matrix_OD_candidates_to_id = "to_id",
  matrix_OD_candidates_dist = "travel_time_p50",
  matrix_OD_existing_site = bixi_od_existing,
  matrix_OD_existing_site_from_id = "from_id",
  matrix_OD_existing_site_to_id = "to_id",
  matrix_OD_existing_site_dist = "travel_time_p50",
  cutoff_distance = 130,
  service_radius = 15, p_facilities = 10
)
#> Warning in mclp(demand = bixi_demand, demand_id = "id", demand_weight =
#> "weight", : 25 id(s) appear in both `candidate` and `existing_sites`; excluding
#> them from `candidate` since they are already open: 1075, 1076, 1077, 1078,
#> 1079, 1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 1090, 1091,
#> 1092, 1093, 1094, 1095, 1096, 1097, 1098, 1099
#> MCLP | 176 demand points | 5786 candidates | radius = 15 | p = 10 | 25 existing (forced) | solver: glpk
res
#> 
#> =======================================================
#>   Model         : MCLP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 35
#>   Covered demand   : 67050.00
#> =======================================================
```

10 new stations plus the 25 existing ones cover 67,050 of the 74,200
total weighted demand (population + jobs) – about 90%, opening only 10
new sites within a 15-minute walk.

### LSCP

The complement of MCLP: minimizes the *number* of facilities needed to
cover every demand point within `service_radius`, with no facility
budget and no partial coverage allowed – every point must be reached.

``` r
res <- lscp(
  demand = sample_demand, demand_id = "id",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  service_radius = 6
)
#> LSCP | 15 demand points | 12 candidates | radius = 6 | solver: glpk
res
#> 
#> =======================================================
#>   Model         : LSCP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 3
#> =======================================================
```

### UFCLP

Chooses which candidate sites to open, trading off each site’s fixed
opening cost (`candidate_fixed_cost`) against the transport cost of
serving demand from it. Unlike P-Median/MCLP, the number of open
facilities isn’t fixed – it falls out of the cost tradeoff, with no
capacity limit on how much demand a site can serve.

``` r
res <- ufclp(
  demand = sample_demand, demand_id = "id", demand_weight = "weight",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  candidate_fixed_cost = "fixed_cost"
)
#> UFCLP | 15 demand points | 12 candidates | solver: glpk
res
#> 
#> =======================================================
#>   Model         : UFCLP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 7
#>   Total cost       : 3547.22
#> =======================================================
```

### CFLP

Extends UFCLP with a capacity limit per facility (`candidate_capacity`):
the total weighted demand assigned to a site can’t exceed it. On this
dataset the capacity limit binds – CFLP has to open more facilities than
UFCLP’s unconstrained optimum to fit all the demand in.

``` r
res <- cflp(
  demand = sample_demand, demand_id = "id", demand_weight = "weight",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  candidate_fixed_cost = "fixed_cost",
  candidate_capacity = "capacity"
)
#> CFLP | 15 demand points | 12 candidates | solver: glpk
res
#> 
#> =======================================================
#>   Model         : CFLP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 10
#>   Total cost       : 4093.48
#> =======================================================
```

### DP

The Dispersion Problem takes no demand layer at all – it selects
`p_facilities` candidate sites to maximize the *minimum* pairwise
distance between the sites chosen, spreading them out as much as
possible (e.g. siting facilities that shouldn’t cluster together). Since
there’s no bundled candidate-to-candidate distance table, this example
builds one directly from the candidate geometry with
`sf::st_distance()`.

``` r
cand_dist <- as.matrix(sf::st_distance(sample_candidates))
ids <- sample_candidates$id
od_candidates_dp <- data.frame(
  from_id = rep(ids, times = length(ids)),
  to_id   = rep(ids, each = length(ids)),
  distance = as.numeric(cand_dist)
)
od_candidates_dp <- od_candidates_dp[od_candidates_dp$from_id != od_candidates_dp$to_id, ]

res <- dp(
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = od_candidates_dp,
  p_facilities = 4
)
#> DP | 12 candidates | p = 4 | solver: glpk
res
#> 
#> =======================================================
#>   Model         : DP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 4
#>   Min pair distance: 657047.22
#> =======================================================
```

### UFLP

The “reverse” of P-Median: the Undesirable Facility Location Problem
selects exactly `p_facilities` sites to *maximize* the total weighted
distance to demand, pushing undesirable installations (landfills,
industrial sites, etc.) as far as possible from where people live.

``` r
res <- uflp(
  demand = sample_demand, demand_id = "id", demand_weight = "weight",
  candidate = sample_candidates, candidate_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  p_facilities = 3
)
#> UFLP | 15 demand points | 12 candidates (p=3) | solver: glpk
res
#> 
#> =======================================================
#>   Model         : UFLP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 3
#>   Total cost       : 18841.00
#> =======================================================
```

### MAXCAP

A competitive-location model: selects up to `p_facilities` new sites to
maximize the weighted demand captured *away from a competitor*
(`existing_sites`), assuming each demand point patronizes whichever site
– new or competitor’s – is closest to it.

``` r
res <- maxcap(
  demand = sample_demand, demand_id = "id", demand_weight = "weight",
  candidate = sample_candidates, candidate_id = "id",
  existing_sites = sample_existing, existing_sites_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  matrix_OD_existing_site = sample_od_existing,
  p_facilities = 3
)
#> MAXCAP | 15 demand points | 12 candidates (<= 3) | solver: glpk
res
#> 
#> =======================================================
#>   Model         : MAXCAP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 3
#>   Covered demand   : 1725.00
#> =======================================================
```

### PMAXCAP

Extends MAXCAP by making price a decision variable too: the firm
simultaneously chooses where to locate `n_facilities` new sites *and*
what price to charge, to maximize profit against a competitor charging
`competitor_price`. Solved exactly by enumerating the piecewise-linear
profit function’s price breakpoints rather than a nonlinear solver.

``` r
res <- pmaxcap(
  demand = sample_demand, demand_id = "id", demand_weight = "weight",
  candidate = sample_candidates, candidate_id = "id",
  existing_sites = sample_existing, existing_sites_id = "id",
  matrix_OD_candidates = sample_od_candidates,
  matrix_OD_existing_site = sample_od_existing,
  competitor_price = 10, n_facilities = 2
)
#> PMAXCAP | 15 demand points | 12 candidates | n = 2 | 180 breakpoints | solver: glpk
res
#> 
#> =======================================================
#>   Model         : PMAXCAP
#>   Solver status : success
#> -------------------------------------------------------
#>   Facilities open : 2
#>   Covered demand   : 2045.00
#>   Optimal price    : 8.31
#>   Profit           : 16987.56
#> =======================================================
```
