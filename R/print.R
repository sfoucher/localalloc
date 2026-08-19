# Console summary for a model result.
#
# This method is what makes `mclp(...)` show something when you call it at the
# top level: R auto-prints any non-invisible value, and auto-printing an object
# of class `localocal_result` dispatches here. Assigning the call
# (`res <- mclp(...)`) suppresses that, as it does for every R object -- type
# `res`, or wrap the call in parentheses (`(res <- mclp(...))`), to print it.
#
# Beyond the headline numbers it lists the sites the model actually opened,
# since that is the result most callers are after. Long site lists are
# truncated to `n` rows; the full layer is always in `$sf_selected`.
#
# Every field is probed with `is.null()` rather than assumed: models return
# different metrics (`covered_demand` for mclp, `max_distance` for p_center,
# `profit` for pmaxcap...), and `build_result()` lets each one attach whatever
# it likes, so this method must cope with any subset being absent.
#' @export
print.localocal_result <- function(x, ..., n = 10) {
  rule  <- "=======================================================\n"
  thin  <- "-------------------------------------------------------\n"

  cat("\n")
  cat(rule)
  cat(sprintf("  Model         : %s\n", toupper(x$model_type)))
  cat(sprintf("  Solver status : %s\n", x$solver_status))
  cat(thin)

  # Headline metrics. Labels are padded to a common width so the colons line up.
  cat(sprintf("  Facilities open   : %d\n",
              if (!is.null(x$n_open)) x$n_open else nrow(x$sf_selected)))
  if (!is.null(x$n_demand))         cat(sprintf("  Demand points     : %d\n", x$n_demand))
  if (!is.null(x$total_cost))       cat(sprintf("  Total cost        : %.2f\n", x$total_cost))
  if (!is.null(x$max_distance))     cat(sprintf("  Max distance      : %.2f\n", x$max_distance))
  if (!is.null(x$covered_demand))   cat(sprintf("  Covered demand    : %.2f\n", x$covered_demand))
  if (!is.null(x$min_distance))     cat(sprintf("  Min pair distance : %.2f\n", x$min_distance))
  if (!is.null(x$optimal_price))    cat(sprintf("  Optimal price     : %.2f\n", x$optimal_price))
  if (!is.null(x$profit))           cat(sprintf("  Profit            : %.2f\n", x$profit))
  if (!is.null(x$processing_time))  cat(sprintf("  Processing time   : %.2fs\n", x$processing_time))

  # ---- The sites the model opened -----------------------------------------
  # `st_drop_geometry()` keeps the block readable: the geometry column would
  # print a WKT string per row and swamp the ids. Its default method passes a
  # plain data.frame through untouched, so this is safe even when `sf_selected`
  # is not an sf object.
  sel <- x$sf_selected
  if (!is.null(sel) && nrow(sel) > 0) {
    cat(thin)
    n_show <- min(nrow(sel), n)
    cat(sprintf("  Selected sites (%s):\n",
                if (n_show < nrow(sel)) sprintf("%d of %d", n_show, nrow(sel))
                else as.character(nrow(sel))))
    print(utils::head(sf::st_drop_geometry(sel), n_show), row.names = FALSE)
    if (n_show < nrow(sel))
      cat(sprintf("  ... %d more -- see `$sf_selected` for the full layer\n",
                  nrow(sel) - n_show))
  }

  # ---- Demand-to-facility assignments -------------------------------------
  # Only the assignment-style models (p_median, p_center, cflp, ...) carry this.
  # One row per demand point is far too long to print, so it is reduced to a
  # served/unassigned count plus the spread of the assigned distances.
  # `facility_id` is NA for a demand point the solver left unserved, which
  # should only happen on a non-optimal solve.
  if (!is.null(x$assignments) && nrow(x$assignments) > 0) {
    cat(thin)
    d <- x$assignments$distance
    served <- sum(!is.na(x$assignments$facility_id))
    cat(sprintf("  Assignments       : %d served", served))
    if (served < nrow(x$assignments))
      cat(sprintf(", %d unassigned", nrow(x$assignments) - served))
    cat("\n")
    if (any(is.finite(d)))
      cat(sprintf("  Distance          : min %.2f | median %.2f | mean %.2f | max %.2f\n",
                  min(d, na.rm = TRUE), stats::median(d, na.rm = TRUE),
                  mean(d, na.rm = TRUE), max(d, na.rm = TRUE)))
    # Breakdown by facility kind, but only when existing sites took some of the
    # demand -- otherwise every row reads "candidate" and the line says nothing.
    if (!is.null(x$assignments$source) && any(x$assignments$source == "existing", na.rm = TRUE)) {
      by_src <- table(x$assignments$source)
      cat(sprintf("  Served by         : %s\n",
                  paste(sprintf("%s %d", names(by_src), as.integer(by_src)), collapse = " | ")))
    }
  }

  # ---- What else is in the object ------------------------------------------
  # Printed so the caller can discover the remaining components without reading
  # the docs; `model_type`/`solver_status` are omitted since they head the block.
  extra <- setdiff(names(x), c("model_type", "solver_status"))
  if (length(extra) > 0) {
    cat(thin)
    cat(sprintf("  Components: %s\n", paste0("$", extra, collapse = "  ")))
  }

  cat(rule)
  cat("\n")
  invisible(x)
}
