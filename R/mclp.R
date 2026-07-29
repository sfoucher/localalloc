#' Maximal Covering Location Problem ( (MCLP)
#'
#' Trouve le nombre **maximum**  de la demande couverte avec un nombre fixe
#' d’installations et un seuil de distance/temps
#'
#' @param cost_matrix matrix. Matrice n_clients x n_facilities de distances.
#' @param demand numeric. Vecteur de longueur n_clients contenant la demande
#'   de chaque client (population, volume, etc.).
#'   Si \code{NULL}, chaque client a une demande de 1.
#' @param service_radius numeric. Distance maximale acceptable.
#' @param p_facilities integer. Nombre de facilities à ouvrir.
#' @param solver character. Solveur ROI : \code{"glpk"} (défaut)
#'
mclp <- function(cost_matrix,
                 demand         = NULL,
                 service_radius,
                 p_facilities,
                 solver         = "glpk") {

  # ----------------------------------------------------------
  # 1. Validation des entrées
  # ----------------------------------------------------------
  .validate_cost_matrix(cost_matrix)

  if (!is.numeric(service_radius) || length(service_radius) != 1 ||
      service_radius <= 0)
    stop("`service_radius` doit être un nombre positif.")

  if (!is.character(solver) || length(solver) != 1)
    stop("`solver` doit être une chaîne de caractères (ex: 'glpk').")

  n_cli <- nrow(cost_matrix)   # nombre de clients
  n_fac <- ncol(cost_matrix)   # nombre de facilities candidates

  # Demande : si NULL, chaque client vaut 1
  if (is.null(demand)) {
    demand <- rep(1L, n_cli)
  } else {
    if (!is.numeric(demand))
      stop("`demand` doit être un vecteur numérique.")
    if (length(demand) != n_cli)
      stop(sprintf(
        "`demand` doit avoir %d éléments (un par client).", n_cli
      ))
    if (any(demand < 0))
      stop("`demand` ne doit pas contenir de valeurs négatives.")
  }

  if (!is.numeric(p_facilities) || p_facilities < 1)
    stop("`p_facilities` doit être un entier >= 1.")

  p_facilities <- as.integer(p_facilities)

  if (p_facilities > n_fac)
    stop(sprintf(
      "`p_facilities` (%d) ne peut pas dépasser le nombre de facilities (%d).",
      p_facilities, n_fac
    ))


  # ----------------------------------------------------------
  # 2. Matrice de couverture binaire bij
  #    bij[i, j] = 1 si facility j peut couvrir client i
  #    bij[i, j] = 0 sinon
  # ----------------------------------------------------------
  bij <- .make_coverage_matrix(cost_matrix, service_radius)

  # Vérifier que chaque client est couvrable par au moins une facility
  clients_sans_couverture <- which(rowSums(bij) == 0)
  if (length(clients_sans_couverture) > 0)
    stop(sprintf(
      paste0(
        "%d client(s) ne peuvent être couverts par aucune facility ",
        "avec service_radius = %g.\n",
        "Clients concernés : %s\n",
        "Augmente service_radius ou ajoute des facilities candidates."
      ),
      length(clients_sans_couverture),
      service_radius,
      paste(clients_sans_couverture, collapse = ", ")
    ))

  # ----------------------------------------------------------
  # 3. Formulation math avec ompr
  # ----------------------------------------------------------
  # Contrainte de couverture :
  # La demande 𝑖 est considérée comme couverte que si au moins
  # une installation ouverte X[j] est située dans le rayon de
  # couverture
  # Σ_j  bij[i,j] * X[j]  >=  Y[i]
  # Σ_j X[j]  = p_facilities   (budget)
  #
  # Variable binaire :
  #   X[j] = 1 si la facility j est ouverte, 0 sinon
  #   Y[i] = 1 si client i est couvert
  #
  # Objectif (maximisation) :
  # a correspond à demand
  #   max  Σ_i  a[i] * Y[i]
  #

  model <- ompr::MIPModel() |>

    # Facilities : X[j] binaire
    ompr::add_variable(
      X[j],
      j    = 1:n_fac,
      type = "binary"
    ) |>

    # Clients couverts : X[i] binaire
    ompr::add_variable(
      Y[i],
      i    = 1:n_cli,
      type = "binary"
    ) |>

    # Objectif : maximiser la demande couverte
    ompr::set_objective(
      ompr::sum_expr(demand[i] * Y[i], i = 1:n_cli),
      sense = "max"
    ) |>

    # Contrainte budget : exactement p facilities ouvertes
    ompr::add_constraint(
      ompr::sum_expr(X[j], j = 1:n_fac) == p_facilities
    ) |>

    # Contrainte couverture : Z[i] ne peut valoir 1 que si
    # au moins une facility dans le rayon est ouverte
    ompr::add_constraint(
      Y[i] <= ompr::sum_expr(bij[i, j] * X[j], j = 1:n_fac),
      i = 1:n_cli
    )

  # ----------------------------------------------------------
  # 4. Résolution
  # ----------------------------------------------------------
  message(sprintf(
    "MCLP : %d clients, %d facilities candidates, rayon = %g, p = %d",
    n_cli, n_fac, service_radius, p_facilities
  ))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf(
      "Erreur du solveur '%s' : %s", solver, e$message
    ))
  )

  if (result$status != "optimal")
    warning(sprintf(
      "Solution non optimale. Statut : '%s'", result$status
    ))

  # ----------------------------------------------------------
  # 5. Extraction de la solution
  # ----------------------------------------------------------
  # Vecteur binaire : X_vals[j] = 1 si facility j est ouverte
  X_vals <- ompr::get_solution(result, X[j])$value

  # ----------------------------------------------------------
  # 6. Construire et retourner l'objet résultat
  # ----------------------------------------------------------
  .build_result(
    X_vals        = X_vals,
    cost_matrix   = cost_matrix,
    bij           = bij,
    model_type    = "mclp",
    solver_status = result$status,
    demand        = demand
  )
}
