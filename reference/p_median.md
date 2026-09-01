# P-Median Problem

Opens exactly `p_facilities` facilities in total to minimize the total
weighted distance between each demand point and its nearest open
facility. Sites in `existing_sites` are forced open from the start
("Required Facilities") and *count against* `p_facilities`: with `k`
existing sites, only `p_facilities - k` candidates are selected.

\$\$\text{Minimize } z = \sum\_{i=1}^{n} \sum\_{j=1}^{m} a_i d\_{ij}
Y\_{ij}\$\$ \$\$\text{s.t. } \sum\_{j=1}^{m} Y\_{ij} = 1, \\ \forall i
\qquad Y\_{ij} \leq X_j, \\ \forall i,j \qquad \sum\_{j=1}^{m} X_j =
p\$\$ \$\$X_j, Y\_{ij} \in \\0,1\\\$\$ where \\a_i\\ = `demand_weight`,
\\d\_{ij}\\ = distance (OD matrix), \\p\\ = `p_facilities`, and \\j\\
indexes candidate *and* existing sites (the latter fixed at \\X_j =
1\\).

## Usage

``` r
p_median(
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
  cutoff = NULL,
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
  is the primary driver of the objective – candidates are chosen to
  minimize (p_median) or maximize (uflp) total weighted assigned
  distance. Defaults to 1 if NULL.

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

- cutoff:

  numeric or NULL. Maximum impedance, expressed in the units of the OD
  table's distance column – which may hold a distance *or* a travel
  time. Pairs beyond it are dropped. `NULL` (default) means no cutoff.

- p_facilities:

  integer. Total number of open facilities, counting both the
  forced-open `existing_sites` and the candidates selected by the model.
  Must be \>= the number of `existing_sites` and \<= the number of
  candidates plus existing sites.

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localalloc_result`. Its `sf_selected` layer lists
*every* open facility – the selected candidates and the forced-open
`existing_sites` – with a `source` column (`"candidate"` / `"existing"`)
telling them apart.
