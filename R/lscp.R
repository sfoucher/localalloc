#' Location Set Covering Problem (LSCP)
#'
#' @description
#' Finds the minimum number of facilities to open so that every demand
#' point is covered by at least one facility within `service_radius`.
#'
#' \deqn{\text{Minimize } z = \sum_{j=1}^{m} X_j}
#' \deqn{\text{s.t. } \sum_{j=1}^{m} b_{ij} X_j \geq 1, \; \forall i \qquad X_j \in \{0,1\}, \; \forall j}
#' where \eqn{b_{ij} = 1} if site \eqn{j} covers demand \eqn{i} (i.e.
#' \eqn{d_{ij} \leq S}, with \eqn{S} = `service_radius`), 0 otherwise.
#'
#' @param demand sf POINT. Demand points.
#' @param demand_id character. Unique id column in `demand`.
#' @param candidate sf POINT. Candidate facility sites.
#' @param candidate_id character. Unique id column in `candidate`.
#' @param existing_sites sf POINT or NULL. Facilities already open, forced
#'   into the solution.
#' @param existing_sites_id character or NULL.
#' @param matrix_OD_candidates data.frame. Long OD table demand-to-candidate.
#' @param matrix_OD_candidates_from_id character.
#' @param matrix_OD_candidates_to_id character.
#' @param matrix_OD_candidates_dist character.
#' @param matrix_OD_existing_site data.frame or NULL.
#' @param matrix_OD_existing_site_from_id character or NULL.
#' @param matrix_OD_existing_site_to_id character or NULL.
#' @param matrix_OD_existing_site_dist character or NULL.
#' @param service_radius numeric. Maximum acceptable impedance for a facility
#'   to count as covering a demand point, expressed in the units of the OD
#'   table's distance column -- which may hold a distance *or* a travel time.
#'   This is also the cutoff: pairs beyond it get no coefficient at all.
#' @param solver character. `"highs"` (default) or `"glpk"`.
#' @return An object of class `localalloc_result`. Its `sf_selected` layer
#'   lists *every* open facility -- the selected candidates and the
#'   forced-open `existing_sites` -- with a `source` column
#'   (`"candidate"` / `"existing"`) telling them apart.
#' @export
lscp <- function(demand, demand_id,
                  candidate, candidate_id,
                  existing_sites = NULL, existing_sites_id = NULL,
                  matrix_OD_candidates,
                  matrix_OD_candidates_from_id = "from_id",
                  matrix_OD_candidates_to_id = "to_id",
                  matrix_OD_candidates_dist = "distance",
                  matrix_OD_existing_site = NULL,
                  matrix_OD_existing_site_from_id = "from_id",
                  matrix_OD_existing_site_to_id = "to_id",
                  matrix_OD_existing_site_dist = "distance",
                  service_radius,
                  solver = "highs") {
  t0 <- Sys.time()

  validate_sf(candidate, "candidate", candidate_id)
  validate_sf(demand, "demand", demand_id)

  has_existing <- !is.null(existing_sites)
  if (has_existing) {
    if (is.null(existing_sites_id))
      stop("`existing_sites_id` is required when `existing_sites` is supplied.")
    validate_sf(existing_sites, "existing_sites", existing_sites_id)
    if (is.null(matrix_OD_existing_site))
      stop("`matrix_OD_existing_site` is required when `existing_sites` is supplied.")
    collision <- intersect(
      as.character(candidate[[candidate_id]]),
      as.character(existing_sites[[existing_sites_id]])
    )
    if (length(collision) > 0) {
      warning(sprintf(
        "%d id(s) appear in both `candidate` and `existing_sites`; excluding them from `candidate` since they are already open: %s",
        length(collision), paste(collision, collapse = ", ")
      ))
      candidate <- candidate[!as.character(candidate[[candidate_id]]) %in% collision, , drop = FALSE]
    }
  }

  if (!is.numeric(service_radius) || length(service_radius) != 1 || service_radius <= 0)
    stop("`service_radius` must be a single positive number.")

  validate_cost_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                       matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                       name = "matrix_OD_candidates")
  if (has_existing)
    validate_cost_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                         matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                         name = "matrix_OD_existing_site")

  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli <- length(ids_demand)
  n_fac <- length(ids_cand)

  # `service_radius` doubles as the OD cutoff. A pair beyond the radius can only
  # ever yield b_ij = 0, so dropping it here is free and there is no separate
  # cutoff argument: a second threshold could only duplicate the radius or
  # silently override it.
  cost_mat_cand <- od_to_matrix(matrix_OD_candidates, matrix_OD_candidates_from_id,
                                matrix_OD_candidates_to_id, matrix_OD_candidates_dist,
                                service_radius, ids_from = ids_demand, ids_to = ids_cand)

  if (has_existing) {
    ids_exist <- as.character(existing_sites[[existing_sites_id]])
    n_exist <- length(ids_exist)
    cost_mat_exist <- od_to_matrix(matrix_OD_existing_site, matrix_OD_existing_site_from_id,
                                   matrix_OD_existing_site_to_id, matrix_OD_existing_site_dist,
                                   service_radius, ids_from = ids_demand, ids_to = ids_exist)
    ids_all_fac <- c(ids_cand, ids_exist)
    cost_mat_all <- cbind(cost_mat_cand, cost_mat_exist)
  } else {
    ids_all_fac <- ids_cand
    cost_mat_all <- cost_mat_cand
  }
  n_all_fac <- length(ids_all_fac)

  # b_ij = 1 when facility j covers demand i (d_ij <= service_radius), else 0.
  bij <- make_coverage_matrix(cost_mat_all, service_radius)
  # A demand point covered by nothing makes the model infeasible: its coverage
  # row would read `0 >= 1`. Fail here with an actionable message rather than
  # letting the solver return a bare "infeasible".
  uncovered <- which(rowSums(bij) == 0)
  if (length(uncovered) > 0)
    stop(sprintf(
      "%d demand point(s) cannot be covered by any facility at service_radius = %g: %s",
      length(uncovered), service_radius, paste(ids_demand[uncovered], collapse = ", ")
    ))

  message("LSCP | building sparse MIP...")
  # ---- Variable layout: [X] only ------------------------------------------
  # One binary variable per facility, X_j = 1 if facility j is opened. Columns
  # 1..n_fac are the candidates, columns n_fac+1..n_all_fac the existing sites.
  # LSCP needs no assignment variables: covering demand i is fully expressed by
  # "at least one covering facility is open".
  #
  # `cov` is the sparse list of covering pairs -- one row per (i, j) with
  # b_ij = 1. Only those pairs get a coefficient in the constraint matrix, which
  # is what keeps A sparse instead of n_cli x n_fac dense.
  cov <- which(bij == 1, arr.ind = TRUE)
  idx_i <- cov[, 1]; idx_j <- cov[, 2]
  n_vars <- n_all_fac

  # ---- Objective: minimize the number of open facilities -------------------
  # Every facility costs 1, so sum(X_j) is simply the count. This is what makes
  # the solver open as few sites as it can get away with.
  L <- rep(1, n_vars)

  # ---- Coverage rows: sum_j b_ij X_j >= 1, one per demand point ------------
  # Row i holds a 1 in every column whose facility covers i. Reading the row:
  # "of the facilities that could cover demand i, at least one must be open".
  # Together with the objective, this is the whole selection mechanism -- the
  # solver looks for the smallest set of columns that touches every row.
  A <- Matrix::sparseMatrix(i = idx_i, j = idx_j, x = 1, dims = c(n_cli, n_vars))
  dir <- rep(">=", n_cli)
  rhs <- rep(1, n_cli)

  if (has_existing) {
    # ---- Required Facilities: X_j == 1 for each existing site --------------
    # One row per existing site, its single 1 in that site's column, pinning the
    # variable to 1. The site is therefore already open when the coverage rows
    # are evaluated, so the demand it covers is free and the objective only pays
    # for the extra candidates still needed.
    A_force <- Matrix::sparseMatrix(i = seq_len(n_exist), j = n_fac + seq_len(n_exist),
                                    x = 1, dims = c(n_exist, n_vars))
    A <- rbind(A, A_force)
    dir <- c(dir, rep("==", n_exist))
    rhs <- c(rhs, rep(1, n_exist))
  }

  # All variables binary (open / not open).
  types <- rep("B", n_vars)
  lower <- rep(0, n_vars)
  upper <- rep(1, n_vars)

  message(sprintf("LSCP | solving | %d demand points | %d candidates | radius = %g | solver: %s",
                   n_cli, n_fac, service_radius, solver))

  result <- solve_direct(L, A, dir, rhs, types, lower, upper, sense = "min", solver = solver)
  if (!result$optimal)
    warning(sprintf("Non-optimal solution. Status: '%s'", result$status))

  # ---- Decode the solution -------------------------------------------------
  # The solution vector is the X block in the layout above, so position j in it
  # is facility j in `ids_all_fac`. `round()` because a solver reports an
  # integral variable as 0.9999999 rather than exactly 1.
  X_vals <- result$solution
  selected_j <- which(round(X_vals) == 1)
  ids_selected <- ids_all_fac[selected_j]
  # The open set includes the forced-open existing sites; keep only the
  # candidates here, since `bind_selected_sites()` re-appends the existing ones
  # from their own layer (tagged `source = "existing"`).
  ids_selected_cand <- intersect(ids_selected, ids_cand)

  sf_selected <- bind_selected_sites(candidate, candidate_id, ids_selected_cand,
                                     if (has_existing) existing_sites else NULL,
                                     existing_sites_id)

  build_result(
    model_type = "lscp", solver_status = result$status, sf_selected = sf_selected,
    n_open = nrow(sf_selected), n_demand = n_cli,
    processing_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}
