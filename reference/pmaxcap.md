# Maximum Capture Problem with Price (PMAXCAP)

Extends
[`maxcap()`](https://sfoucher.github.io/localalloc/reference/maxcap.md)
by making the firm's price a decision variable. For a fixed set of open
sites, profit is piecewise-linear in price (capture only flips at
threshold breakpoints), so the true global optimum is found by
enumerating every breakpoint price, solving the resulting MAXCAP-style
linear MIP at each one, and keeping the best. This needs no
nonlinear/MINLP solver.

\$\$\text{Maximize } \Pi = (P^A - v) \sum\_{i \in I} a_i Y_i^A -
\sum\_{j \in J} f_j X_j^A\$\$ \$\$\text{s.t. } Y_i^A \leq \sum\_{j \in
N_i(b_i^B)} X_j^A, \\ \forall i \qquad \sum\_{j=1}^{m} X_j^A = n^A\$\$
\$\$X_j^A, Y_i^A \in \\0,1\\\$\$ where \\N_i(b_i^B) = \\j \in J : P^A +
t\\d\_{ij} \< P^B + t\\d\_{i,b_i^B}\\\\ (capture zone, depends on price
\\P^A\\), \\P^A\\ = optimized price (returned in `optimal_price`), \\v\\
= `marginal_cost`, \\f_j\\ = `candidate_fixed_cost`, \\n^A\\ =
`n_facilities`, \\P^B\\ = `competitor_price`, \\t\\ =
`distance_cost_rate`. Solved by enumerating price breakpoints (see
above), not directly as a MINLP.

## Usage

``` r
pmaxcap(
  demand,
  demand_id,
  demand_weight = NULL,
  candidate,
  candidate_id,
  existing_sites,
  existing_sites_id,
  matrix_OD_candidates,
  matrix_OD_candidates_from_id = "from_id",
  matrix_OD_candidates_to_id = "to_id",
  matrix_OD_candidates_dist = "distance",
  matrix_OD_existing_site,
  matrix_OD_existing_site_from_id = "from_id",
  matrix_OD_existing_site_to_id = "to_id",
  matrix_OD_existing_site_dist = "distance",
  cutoff_distance = NULL,
  marginal_cost = 0,
  distance_cost_rate = 1,
  competitor_price,
  n_facilities,
  candidate_fixed_cost = NULL,
  max_breakpoints = 2000,
  solver = "highs"
)
```

## Arguments

- demand:

  sf POINT. Demand points.

- demand_id:

  character. Unique id column in `demand`.

- demand_weight:

  character or NULL. Weight column in `demand` (e.g. population). This
  drives the objective – captured demand is weighted by this column.
  Defaults to 1 if NULL.

- candidate:

  sf POINT. Candidate facility sites.

- candidate_id:

  character. Unique id column in `candidate`.

- existing_sites:

  sf POINT. The competitor's sites (required).

- existing_sites_id:

  character. Unique id column in `existing_sites`.

- matrix_OD_candidates:

  data.frame. Long OD table demand-to-candidate.

- matrix_OD_candidates_from_id:

  character.

- matrix_OD_candidates_to_id:

  character.

- matrix_OD_candidates_dist:

  character.

- matrix_OD_existing_site:

  data.frame. Required.

- matrix_OD_existing_site_from_id:

  character or NULL.

- matrix_OD_existing_site_to_id:

  character or NULL.

- matrix_OD_existing_site_dist:

  character or NULL.

- cutoff_distance:

  numeric or NULL. Pairs beyond this distance are dropped. `NULL`
  (default) means no cutoff.

- marginal_cost:

  numeric. Marginal production cost per unit (v).

- distance_cost_rate:

  numeric. Cost per unit distance (t).

- competitor_price:

  numeric. The competitor's fixed price (P_B).

- n_facilities:

  integer. Exact number of new facilities to open (n_A).

- candidate_fixed_cost:

  character or NULL. Column in `candidate` for the fixed cost of opening
  each site (f_j, eq. 2.36). Defaults to 0 if NULL.

- max_breakpoints:

  integer. Caps the number of price breakpoints evaluated. Under the
  cap: exact. Over it: breakpoints are subsampled and a warning is
  raised – the result is then an approximation, not the exact global
  optimum. Default 2000.

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localalloc_result`, with `optimal_price` and
`profit` fields in addition to the usual ones.
