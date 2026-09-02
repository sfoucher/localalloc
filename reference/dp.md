# Dispersion Problem (DP)

Selects `p_facilities` candidate sites to maximize the minimum pairwise
distance between opened sites (maximally spread-out placement). Takes no
demand layer – it's a pure site-dispersion problem among `candidate`s.

\$\$\text{Maximize } D\$\$ \$\$\text{s.t. } D \leq d\_{ij} + (M -
d\_{ij})(1-X_i) + (M-d\_{ij})(1-X_j), \\ \forall i,j\$\$ \$\$\sum\_{j}
X_j = P \qquad X_j \in \\0,1\\\$\$ where \\D\\ = `min_distance` in the
output, \\P\\ = `p_facilities`, and \\M\\ is a sufficiently large
constant (computed internally as 2 times the maximum distance in the
matrix).

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

  sf POINT. Candidate sites. Its row order is not neutral when the
  distance matrix is asymmetric; see Details.

- candidate_id:

  character. Unique id column in `candidate`.

- matrix_OD_candidates:

  data.frame. Long candidate-to-candidate distance table
  (from_id/to_id/distance) – both directions of each pair should be
  supplied. Pairs missing from the upper triangle are warned about and
  given a large finite distance; see Details for how directions that
  disagree are resolved.

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

An object of class `localalloc_result`.

## Asymmetric matrices: the row order of `candidate` affects the answer

The dispersion constraint is symmetric in \\(i, j)\\, so the model
builds one row per *unordered* pair and reads only the upper triangle of
the distance matrix. When a pair disagrees between its two directions –
`A -> B = 10` but `B -> A = 15`, routine with travel times because of
one-way streets and slope – the two values are not combined, averaged or
reconciled. **One of them is silently discarded**, and which one depends
on the order the sites appear in `candidate`: the value kept is the one
whose `from_id` comes first.

This is not flagged by any check, because an asymmetric matrix is
legitimate data. But it does mean an incidental reordering upstream (a
`candidate[order(candidate$id), ]`, a different `dplyr` pipeline) can
change both `min_distance` and the sites returned, on identical input:

    # candidate order A,B,C  ->  sites B,C  |  min_distance = 5750
    # candidate order A,C,B  ->  sites A,C  |  min_distance = 4950

If your matrix is asymmetric, symmetrize it yourself before calling
`dp()` so the choice is explicit rather than an artefact of row order.
Keeping the **smaller** of the two directions is the conservative
option: `min_distance` then reads as a guarantee – no two selected sites
are closer than \\D\\ – that holds whichever way each pair is travelled.
