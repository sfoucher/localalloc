# Maximal Covering Location Problem (MCLP)

Opens `p_facilities` facilities in total to maximize the weighted demand
covered within `service_radius`. Unlike
[`lscp()`](https://sfoucher.github.io/localloc/reference/lscp.md), full
coverage of every demand point is not required – demand that can't be
reached by any facility within the radius is simply left uncovered,
which is the point of "maximal" coverage rather than total coverage.
`existing_sites`, if supplied, are forced open ("Required Facilities",
same semantics as
[`p_median()`](https://sfoucher.github.io/localloc/reference/p_median.md)/[`p_center()`](https://sfoucher.github.io/localloc/reference/p_center.md))
and *count against* `p_facilities`: with `k` existing sites, only
`p_facilities - k` candidates are selected.

\$\$\text{Maximize } z = \sum\_{i=1}^{n} a_i Y_i\$\$ \$\$\text{s.t. }
\sum\_{j=1}^{m} b\_{ij} X_j \geq Y_i, \\ \forall i \qquad
\sum\_{j=1}^{m} X_j = p\$\$ \$\$X_j, Y_i \in \\0,1\\\$\$ where \\a_i\\ =
`demand_weight`, \\b\_{ij} = 1\\ if \\d\_{ij} \leq S\\
(`service_radius`), \\p\\ = `p_facilities`.

## Usage

``` r
mclp(
  demand,
  demand_id,
  demand_weight = NULL,
  candidate,
  candidate_id,
  existing_sites = NULL,
  existing_sites_id = NULL,
  matrix_OD_candidates,
  matrix_OD_candidates_from_id = "from_id",
  matrix_OD_candidates_to_id = "to_id",
  matrix_OD_candidates_dist = "distance",
  matrix_OD_existing_site = NULL,
  matrix_OD_existing_site_from_id = "from_id",
  matrix_OD_existing_site_to_id = "to_id",
  matrix_OD_existing_site_dist = "distance",
  cutoff_distance = NULL,
  service_radius,
  p_facilities,
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
  is the primary driver of MCLP's objective – candidates are chosen to
  maximize total weighted covered demand. Defaults to 1 if NULL.

- candidate:

  sf POINT. Candidate facility sites.

- candidate_id:

  character. Unique id column in `candidate`.

- existing_sites:

  sf POINT or NULL. Facilities already open, forced into the solution.

- existing_sites_id:

  character or NULL.

- matrix_OD_candidates:

  data.frame. Long OD table demand-to-candidate.

- matrix_OD_candidates_from_id:

  character.

- matrix_OD_candidates_to_id:

  character.

- matrix_OD_candidates_dist:

  character.

- matrix_OD_existing_site:

  data.frame or NULL.

- matrix_OD_existing_site_from_id:

  character or NULL.

- matrix_OD_existing_site_to_id:

  character or NULL.

- matrix_OD_existing_site_dist:

  character or NULL.

- cutoff_distance:

  numeric or NULL. Pairs beyond this distance are dropped. `NULL`
  (default) means no cutoff.

- service_radius:

  numeric. Maximum acceptable distance.

- p_facilities:

  integer. Total number of open facilities, counting both the
  forced-open `existing_sites` and the candidates selected by the model.
  Must be \>= the number of `existing_sites` and \<= the number of
  candidates plus existing sites.

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localloc_result`. Its `sf_selected` layer lists
*every* open facility – the selected candidates and the forced-open
`existing_sites` – with a `source` column (`"candidate"` / `"existing"`)
telling them apart.
