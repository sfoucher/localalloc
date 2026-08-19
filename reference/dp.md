# Dispersion Problem (DP)

Selects `p_facilities` candidate sites to maximize the minimum pairwise
distance between opened sites (maximally spread-out placement). Takes no
demand layer – it's a pure site-dispersion problem among `candidate`s.

## Usage

``` r
dp(
  candidate,
  candidate_id,
  matrix_OD_candidates,
  matrix_OD_candidates_from_id = "from_id",
  matrix_OD_candidates_to_id = "to_id",
  matrix_OD_candidates_dist = "distance",
  p_facilities,
  solver = "highs"
)
```

## Arguments

- candidate:

  sf POINT. Candidate sites.

- candidate_id:

  character. Unique id column in `candidate`.

- matrix_OD_candidates:

  data.frame. Long candidate-to-candidate distance table
  (from_id/to_id/distance) – both directions of each pair should be
  supplied.

- matrix_OD_candidates_from_id:

  character.

- matrix_OD_candidates_to_id:

  character.

- matrix_OD_candidates_dist:

  character.

- p_facilities:

  integer. Number of sites to select (\>= 2).

- solver:

  character. `"highs"` (default) or `"glpk"`.

## Value

An object of class `localloc_result`.

## Details

\$\$\text{Maximize } D\$\$ \$\$\text{s.t. } D \leq d\_{ij} + (M -
d\_{ij})(1-X_i) + (M-d\_{ij})(1-X_j), \\ \forall i,j\$\$ \$\$\sum\_{j}
X_j = P \qquad X_j \in \\0,1\\\$\$ where \\D\\ = `min_distance` in the
output, \\P\\ = `p_facilities`, and \\M\\ is a sufficiently large
constant (computed internally as 2 times the maximum distance in the
matrix).
