# Interactive map of facility-location input data

Plots candidate sites, demand points, and (optionally) existing sites on
an interactive `mapview`/leaflet map. Demand points are colored by their
weight column when present.

## Usage

``` r
plot_sites(
  candidate,
  demand = NULL,
  existing_sites = NULL,
  demand_weight = "weight"
)
```

## Arguments

- candidate:

  sf POINT layer of candidate sites.

- demand:

  Optional sf POINT layer of demand points.

- existing_sites:

  Optional sf POINT layer of existing/forced-open sites.

- demand_weight:

  Column name in `demand` used to color points (default `"weight"`);
  ignored if the column doesn't exist.

## Value

A `mapview` object.
