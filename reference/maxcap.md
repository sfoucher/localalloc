# Maximum Capture Problem (MAXCAP)

Selects at most `p_facilities` candidate sites to maximize the weighted
demand captured from a competitor (`existing_sites`), assuming each
demand point currently patronizes its nearest existing site and switches
to any newly opened candidate that is closer.

Reduced formulation: omits the tie-breaking mechanism between a
candidate and the incumbent competitor (assumes no exact-distance ties).
\$\$\text{Maximize } z = \sum\_{i \in I} w_i Y_i\$\$ \$\$\text{s.t. }
Y_i \leq \sum\_{j \in p_i} X_j, \\ \forall i \qquad \sum\_{j \in J} X_j
\leq P\$\$ \$\$X_j, Y_i \in \\0,1\\\$\$ where \\w_i\\ = `demand_weight`,
\\P\\ = `p_facilities`, and \\p_i\\ is the set of candidate sites closer
to \\i\\ than its nearest competitor.

## Usage

``` r
maxcap(
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

- cutoff:

  numeric or NULL. Maximum impedance, expressed in the units of the OD
  table's distance column – which may hold a distance *or* a travel
  time. Pairs beyond it are dropped. `NULL` (default) means no cutoff.

- p_facilities:

  integer. Maximum number of new facilities to open (the budget
  constraint is `<=`, not `=`, per eq. 2.35).

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localalloc_result`.
