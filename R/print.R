#' @export
print.llocalocal_result <- function(x, ...) {
  cat("\n")
  cat("=======================================================\n")
  cat(sprintf("  Model         : %s\n", toupper(x$model_type)))
  cat(sprintf("  Solver status : %s\n", x$solver_status))
  cat("-------------------------------------------------------\n")
  cat(sprintf("  Facilities open : %d\n", if (!is.null(x$n_open)) x$n_open else nrow(x$sf_selected)))
  if (!is.null(x$total_cost))       cat(sprintf("  Total cost       : %.2f\n", x$total_cost))
  if (!is.null(x$max_distance))     cat(sprintf("  Max distance     : %.2f\n", x$max_distance))
  if (!is.null(x$covered_demand))   cat(sprintf("  Covered demand   : %.2f\n", x$covered_demand))
  if (!is.null(x$min_distance))     cat(sprintf("  Min pair distance: %.2f\n", x$min_distance))
  if (!is.null(x$optimal_price))    cat(sprintf("  Optimal price    : %.2f\n", x$optimal_price))
  if (!is.null(x$profit))           cat(sprintf("  Profit           : %.2f\n", x$profit))
  cat("=======================================================\n\n")
  invisible(x)
}
