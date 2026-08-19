# Uncapacitated Fixed-charge Facility Location Problem (UFCLP)

Chooses which candidate sites to open, trading off each site's fixed
opening cost against the transport cost of serving demand from it.
Unlike
[`p_median()`](https://sfoucher.github.io/localloc/reference/p_median.md)/[`mclp()`](https://sfoucher.github.io/localloc/reference/mclp.md),
the number of open facilities is not fixed – it falls out of the cost
tradeoff.

## Usage

``` r
ufclp(
  demand,
  demand_id,
  demand_weight = NULL,
  candidate,
  candidate_id,
  matrix_OD_candidates,
  matrix_OD_candidates_from_id = "from_id",
  matrix_OD_candidates_to_id = "to_id",
  matrix_OD_candidates_dist = "distance",
  cutoff_distance = NULL,
  candidate_fixed_cost,
  transport_cost_rate = 1,
  solver = "highs"
)
```

## Arguments

- demand:

  sf POINT. Demand points.

- demand_id:

  character. Unique id column in `demand`.

- demand_weight:

  character or NULL. Weight column in `demand`.

- candidate:

  sf POINT. Candidate facility sites.

- candidate_id:

  character. Unique id column in `candidate`.

- matrix_OD_candidates:

  data.frame. Long OD table demand-to-candidate.

- matrix_OD_candidates_from_id:

  character.

- matrix_OD_candidates_to_id:

  character.

- matrix_OD_candidates_dist:

  character.

- cutoff_distance:

  numeric or NULL. Pairs beyond this distance are dropped. `NULL`
  (default) means no cutoff.

- candidate_fixed_cost:

  character. Column in `candidate` holding the fixed cost of opening
  each site (f_j).

- transport_cost_rate:

  numeric. Cost per unit distance per unit demand (alpha). Default 1.

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localloc_result`.

## Details

\$\$\text{Minimize } z = \sum\_{j=1}^{m} f_j X_j + \alpha
\sum\_{i=1}^{n} \sum\_{j=1}^{m} a_i d\_{ij} Y\_{ij}\$\$ \$\$\text{s.t. }
\sum\_{j=1}^{m} Y\_{ij} = 1, \\ \forall i \qquad Y\_{ij} \leq X_j, \\
\forall i,j\$\$ \$\$X_j \in \\0,1\\ \qquad Y\_{ij} \geq 0\$\$ where
\\f_j\\ = `candidate_fixed_cost`, \\\alpha\\ = `transport_cost_rate`,
\\a_i\\ = `demand_weight`, \\d\_{ij}\\ = distance (OD matrix).
