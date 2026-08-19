# Documentation-only file: no code runs here. Each block below documents one
# bundled dataset (the .rda files in `data/`, produced by the scripts in
# `data-raw/`), and the bare string that closes each block -- `"sample_demand"`,
# `"bixi_candidates"` -- is roxygen's way of attaching the docs to a data object
# rather than to a function. Related datasets are grouped under a single topic via
# `@name`/`@aliases`, so `?bixi_demand` and `?bixi_od_existing` both land on the
# same page.

#' Small synthetic facility-location dataset
#'
#' A synthetic dataset of demand points, candidate sites, and an existing
#' site with matching OD tables, small enough to solve instantly. Used in
#' examples, the vignette, and test fixtures.
#'
#' @format `sample_demand`, `sample_candidates`, `sample_existing`: sf
#'   POINT objects. `sample_od_candidates`, `sample_od_existing`:
#'   data.frames with `from_id`, `to_id`, `distance` columns.
#' @name sample-data
#' @aliases sample_demand sample_candidates sample_existing sample_od_candidates sample_od_existing
"sample_demand"

#' Sherbrooke Bixi bike-share dataset
#'
#' The real-world case study from the thesis this package originates from
#' (`Essai_MarieHelene.pdf`): candidate/existing Bixi bike-share station
#' sites and demand in the Fleurimont/Nations boroughs of Sherbrooke, QC,
#' with OD walking-time matrices computed via `r5r`.
#'
#' `bixi_candidates` (5,811 pts): candidate sites from OSM street segments
#' (cycleway/living_street/footway/residential), cut into 100 m lixels,
#' excluding slope > 5%.
#'
#' `bixi_existing` (25 pts): the real Bixi stations (open data from BIXI).
#'
#' `bixi_demand` (176 pts): 2021-census dissemination-area centroids;
#' `weight` = population aged 15-64 at residence **plus** jobs at
#' workplace. **Licensing note:** the jobs-at-workplace figure comes from
#' a special compilation purchased from Statistics Canada, not open data,
#' and is already summed into `weight` (the two components can't be
#' separated post hoc). Bundled here on the package maintainer's authority
#' over that data's usage terms -- check before redistributing this
#' dataset further.
#'
#' `bixi_od_candidates` / `bixi_od_existing`: long OD tables (`from_id`,
#' `to_id`, `travel_time_p50`) of walking travel times.
#'
#' @format See Details.
#' @name bixi-data
#' @aliases bixi_candidates bixi_demand bixi_existing bixi_od_candidates bixi_od_existing
"bixi_candidates"
