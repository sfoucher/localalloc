# Capacitated Fixed-charge Facility Location Problem (CFLP)

Extends
[`ufclp()`](https://sfoucher.github.io/localalloc/reference/ufclp.md)
with a capacity limit per facility: the total weighted demand assigned
to a site cannot exceed its capacity.

Same objective and base constraints as
[`ufclp()`](https://sfoucher.github.io/localalloc/reference/ufclp.md),
plus: \$\$\sum\_{i=1}^{n} a_i Y\_{ij} \leq k_j X_j, \\ \forall j\$\$
where \\k_j\\ = `candidate_capacity`.

## Usage

``` r
cflp(
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
  candidate_capacity,
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

- candidate_capacity:

  character. Column in `candidate` holding each site's maximum capacity
  (k_j).

- transport_cost_rate:

  numeric. Cost per unit distance per unit demand (alpha). Default 1.

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localalloc_result`.
