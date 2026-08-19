# Small synthetic facility-location dataset

A synthetic dataset of demand points, candidate sites, and an existing
site with matching OD tables, small enough to solve instantly. Used in
examples, the vignette, and test fixtures.

## Usage

``` r
sample_demand
```

## Format

`sample_demand`, `sample_candidates`, `sample_existing`: sf POINT
objects. `sample_od_candidates`, `sample_od_existing`: data.frames with
`from_id`, `to_id`, `distance` columns.
