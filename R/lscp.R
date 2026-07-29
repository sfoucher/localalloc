#' Location Set Covering Problem (LSCP)
#'
#' Trouve le nombre **minimum** de facilities à ouvrir pour que
#' chaque client soit couvert par au moins une facility dans un
#' rayon de service donné.
#'
#' @param cost_matrix matrix. Matrice n_clients x n_facilities de distances.
#'   Produite par utilisateur.
#' @param service_radius numeric. Distance maximale acceptable entre
#'   un client et sa facility.
#' @param solver character. Solveur ROI : \code{"glpk"} (défaut)
#'
#'
#'
#'
lscp <- function(cost_matrix,
                 service_radius,
                 solver = "glpk") {

  # ----------------------------------------------------------
  # 1. Validation des entrées
  # ----------------------------------------------------------
  .validate_cost_matrix(cost_matrix)

  if (!is.numeric(service_radius) || length(service_radius) != 1 ||
      service_radius <= 0)
    stop("`service_radius` doit être un nombre positif.")

  if (!is.character(solver) || length(solver) != 1)
    stop("`solver` doit être une chaîne de caractères (ex: 'glpk').")

  # ----------------------------------------------------------
  # 2. Dimensions
  # ----------------------------------------------------------
  n_cli <- nrow(cost_matrix)   # nombre de clients
  n_fac <- ncol(cost_matrix)   # nombre de facilities candidates

  # ----------------------------------------------------------
  # 3. Matrice de couverture binaire bij
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
  # 4. Formulation math avec ompr
  # ----------------------------------------------------------
  # Contrainte de couverture :
  # La demande 𝑖 est considérée comme couverte que si au moins
  # une installation ouverte X[j] est située dans le rayon de
  # couverture
  # Σ_j  bij[i,j] * X[j]  >=  1
  #
  # Variable binaire :
  #   X[j] = 1 si la facility j est ouverte, 0 sinon
  #
  # Objectif (minimisation) :
  #   min z = Σ_j  X[j]
  #

  # Création modèle avec les variable, contrainte et couverture
  model <- ompr::MIPModel() |>

    # Variables binaires X[j] pour chaque facility candidate
    ompr::add_variable(
      X[j],
      j    = 1:n_fac,
      type = "binary"
    ) |>

    # Objectif : minimiser le nombre de facilities ouvertes
    ompr::set_objective(
      ompr::sum_expr(X[j], j = 1:n_fac),
      sense = "min"
    ) |>

    # Contrainte : chaque client doit être couvert
    ompr::add_constraint(
      ompr::sum_expr(bij[i, j] * X[j], j = 1:n_fac) >= 1,
      i = 1:n_cli
    )

  # ----------------------------------------------------------
  # 5. Résolution
  # ----------------------------------------------------------
  message(sprintf(
    "LSCP : %d clients, %d facilities candidates, rayon = %g",
    n_cli, n_fac, service_radius
  ))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) {
      stop(sprintf(
        "Erreur du solveur '%s' : %s\nVérifie que ROI.plugin.%s est installé.",
        solver, e$message, solver
      ))
    }
  )

  # Vérifier le statut de la solution
  if (result$status != "optimal")
    warning(sprintf(
      "Le solveur n'a pas trouvé de solution optimale. Statut : '%s'",
      result$status
    ))

  # ----------------------------------------------------------
  # 6. Extraction de la solution
  # ----------------------------------------------------------
  # Vecteur binaire : X_vals[j] = 1 si facility j est ouverte
  X_vals <- ompr::get_solution(result, X[j])$value

  # ----------------------------------------------------------
  # 7. Construire et retourner l'objet résultat
  # ----------------------------------------------------------
  .build_result(
    X_vals        = X_vals,
    cost_matrix   = cost_matrix,
    bij           = bij,
    model_type    = "lscp",
    solver_status = result$status
  )
}
