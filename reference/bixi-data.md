# Sherbrooke Bixi bike-share dataset

The real-world case study from the thesis this package originates from
(`Essai_MarieHelene.pdf`): candidate/existing Bixi bike-share station
sites and demand in the Fleurimont/Nations boroughs of Sherbrooke, QC,
with OD walking-time matrices computed via `r5r`.

## Usage

``` r
bixi_candidates
```

## Format

See Details.

## Details

`bixi_candidates` (2,743 pts): candidate sites from OSM street segments
(cycleway/living_street/footway/residential), cut into 100 m lixels,
excluding slope \> 5%.

`bixi_existing` (25 pts): the real Bixi stations (open data from BIXI).

`bixi_demand` (176 pts): 2021-census dissemination-area centroids;
`weight` = population aged 15-64 at residence **plus** jobs at
workplace. **Licensing note:** the jobs-at-workplace figure comes from a
special compilation purchased from Statistics Canada, not open data, and
is already summed into `weight` (the two components can't be separated
post hoc). Bundled here on the package maintainer's authority over that
data's usage terms – check before redistributing this dataset further.

`bixi_od_candidates` / `bixi_od_existing`: long OD tables (`from_id`,
`to_id`, `travel_time_p50`) of walking travel times.
