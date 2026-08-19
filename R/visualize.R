#' Interactive map of facility-location input data
#'
#' @description
#' Plots candidate sites, demand points, and (optionally) existing sites on
#' an interactive `mapview`/leaflet map. Demand points are colored by their
#' weight column when present.
#'
#' @param candidate sf POINT layer of candidate sites.
#' @param demand Optional sf POINT layer of demand points.
#' @param existing_sites Optional sf POINT layer of existing/forced-open sites.
#' @param demand_weight Column name in `demand` used to color points
#'   (default `"weight"`); ignored if the column doesn't exist.
#'
#' @return A `mapview` object.
#' @export
plot_sites <- function(candidate, demand = NULL, existing_sites = NULL,
                        demand_weight = "weight") {
  # `mapview` is a Suggests, not an Imports: it drags in the whole leaflet stack,
  # which nothing else in the package needs, so it is checked at call time and the
  # message names the install command rather than failing on a missing namespace.
  if (!requireNamespace("mapview", quietly = TRUE))
    stop("Package 'mapview' is required for `plot_sites()`. Install it with install.packages(\"mapview\").")
  if (!inherits(candidate, "sf"))
    stop("`candidate` must be an sf object.")

  # `candidate` is the only required layer and is drawn first, small and grey:
  # there are typically thousands of them and they are the backdrop, not the
  # subject. `mapview` objects compose with `+`, each call adding a toggleable
  # layer, so the rest of the function just appends to `m`.
  m <- mapview::mapview(candidate, col.regions = "grey60", cex = 3,
                         layer.name = "Candidates")

  if (!is.null(demand)) {
    if (!inherits(demand, "sf"))
      stop("`demand` must be an sf object.")
    # Colour demand by weight when that column exists. `zcol = NULL` is mapview's
    # "single flat colour", so an absent weight column silently degrades to a
    # plain layer instead of erroring -- the column is a convenience here, not a
    # requirement, and the models tolerate its absence too.
    zcol <- if (demand_weight %in% names(demand)) demand_weight else NULL
    m <- m + mapview::mapview(demand, zcol = zcol, layer.name = "Demand")
  }

  if (!is.null(existing_sites)) {
    if (!inherits(existing_sites, "sf"))
      stop("`existing_sites` must be an sf object.")
    # Red and twice the size: existing sites are few and matter most to read at a
    # glance -- either the facilities forced open, or (maxcap/pmaxcap) the
    # competitor's network.
    m <- m + mapview::mapview(existing_sites, col.regions = "red", cex = 6,
                               layer.name = "Existing sites")
  }

  m
}
