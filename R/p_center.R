#' P-Center Problem
#'
#' Sélectionne exactement le nombre de facilities pour
#' **minimiser la distance maximale** entre un client et sa
#' facility la plus proche (critère minimax).
#'
#' @param cost_matrix matrix. Matrice n_clients x n_facilities de distances.
#' @param p_facilities integer. Nombre de facilities à ouvrir.
#' @param solver character. Solveur ROI : \code{"glpk"} (défaut)
#'
p_center <- function(cost_matrix,
                     p_facilities,
                     solver = "glpk") {

  # ----------------------------------------------------------
  # 1. Validation des entrées
  # ----------------------------------------------------------
  .validate_cost_matrix(cost_matrix)

  n_cli <- nrow(cost_matrix)
  n_fac <- ncol(cost_matrix)

  if (!is.numeric(p_facilities) || p_facilities < 1)
    stop("`p_facilities` doit être un entier >= 1.")

  p_facilities <- as.integer(p_facilities)

  if (p_facilities > n_fac)
    stop(sprintf(
      "`p_facilities` (%d) ne peut pas dépasser le nombre de facilities (%d).",
      p_facilities, n_fac
    ))

  # ----------------------------------------------------------
  # 3. Formulation math avec ompr
  # ----------------------------------------------------------
  # Contraintes:
  #   Σ_j X[j] = p_facilities    (budget)
  #   Σ_j Y[i,j] = 1    ∀i        (chaque client assigné à 1 facility)
  #   Y[i,j] <= X[j] ∀i,j      (client assigné seulement si facility ouverte)
  #
  # Variable binaire :
  #   X[j] = 1 si la facility j est ouverte, 0 sinon
  #   Y[i,j]  = 1 si client i est assigné à facility j
  #
  # Objectif (minimisation) :
  #   min z
  #   z ≥ cost_matrix[i,j] * Y[i,j]
  #

  # Création modèle avec les variable, contrainte et couverture
  model <- ompr::MIPModel() |>

    # Facilities ouvertes
    ompr::add_variable(
      X[j],
      j    = 1:n_fac,
      type = "binary"
    ) |>

    # Assignation client-facility
    ompr::add_variable(
      Y[i, j],
      i    = 1:n_cli,
      j    = 1:n_fac,
      type = "binary"
    ) |>

    # Variable auxiliaire Z : distance maximale (continue)
    ompr::add_variable(
      Z,
      type = "continuous",
      lb   = 0   # borne inférieure = 0
    ) |>

    # Objectif : minimiser Z
    ompr::set_objective(Z, sense = "min") |>

    # Contrainte budget
    ompr::add_constraint(
      ompr::sum_expr(X[j], j = 1:n_fac) == p_facilities
    ) |>

    # Chaque client assigné à exactement 1 facility
    ompr::add_constraint(
      ompr::sum_expr(Y[i, j], j = 1:n_fac) == 1,
      i = 1:n_cli
    ) |>

    # Un client ne peut être assigné qu'à une facility ouverte
    ompr::add_constraint(
      Y[i, j] <= X[j],
      i = 1:n_cli,
      j = 1:n_fac
    ) |>

    # Z doit être >= à la distance de chaque client à sa facility
    ompr::add_constraint(
      ompr::sum_expr(cost_matrix[i, j] * Y[i, j], j = 1:n_fac) <= Z,
      i = 1:n_cli
    )

  # ----------------------------------------------------------
  # 3. Résolution
  # ----------------------------------------------------------
  message(sprintf(
    "P-Center : %d clients, %d facilities candidates, p = %d",
    n_cli, n_fac, p_facilities
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
  # 4. Extraction de la solution
  # ----------------------------------------------------------
  X_vals      <- ompr::get_solution(result, X[j])$value
  Y_vals      <- ompr::get_solution(result, Y[i, j])
  max_distance <- ompr::get_solution(result, Z)$value


  # ----------------------------------------------------------
  # 5. Construire et retourner l'objet résultat
  # ----------------------------------------------------------
  # Construire le résultat
  .build_result(
    X_vals        = X_vals,
    cost_matrix   = cost_matrix,
    bij           = matrix(1L, nrow = n_cli, ncol = n_fac),
    model_type    = "p_center",
    solver_status = result$status
  )
}
