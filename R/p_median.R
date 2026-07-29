#' P-Median Problem
#'
#' Sélectionne exactement le nombre facilities pour
#' **minimiser la distance totale pondérée** entre chaque client
#' et sa facility la plus proche.
#' Les sites existants (\code{existing_sites}) sont  ouverts dans le ILP
#' dès le départ.
#'
#' @param candidate        sf POINT. Sites candidats à évaluer.
#' @param candidate_id     character. Colonne d'identifiant unique dans
#'                         \code{candidate}.
#' @param candidate_weight character ou NULL. Colonne de poids dans
#'                         \code{candidate}. Si NULL, poids = 1.
#' @param existing_sites        sf POINT ou NULL. Sites déjà ouverts,
#'                              forcés dans la solution (Required Facilities).
#' @param existing_sites_id     character ou NULL. Colonne d'identifiant
#'                              unique dans \code{existing_sites}.
#' @param existing_sites_weight character ou NULL. Colonne de poids dans
#'                              \code{existing_sites}. Si NULL, poids = 1.
#' @param demand        sf POINT. Points de demande (clients).
#' @param demand_id     character. Colonne d'identifiant unique dans
#'                      \code{demand}.
#' @param demand_weight character ou NULL. Colonne de poids (population,
#'                      volume…) dans \code{demand}. Si NULL, poids = 1.
#'
#' @param matrix_OD_candidates         data.frame. Matrice OD longue entre
#'                                     demande et candidates.
#' @param matrix_OD_candidates_from_id character. Colonne origine (id demande).
#' @param matrix_OD_candidates_to_id   character. Colonne destination (id candidat).
#' @param matrix_OD_candidates_dist    character. Colonne distance.
#'
#' @param matrix_OD_existing_site          data.frame ou NULL. Matrice OD longue
#'                                         entre demande et sites existants.
#'                                         Obligatoire si \code{existing_sites}
#'                                         est fourni.
#' @param matrix_OD_existing_site_from_id  character ou NULL. Colonne origine.
#' @param matrix_OD_existing_site_to_id    character ou NULL. Colonne destination.
#' @param matrix_OD_existing_site_dist     character ou NULL. Colonne distance.
#'
#' @param cutoff_distance numeric. Distance maximale au-delà de laquelle une
#'                        paire demande-facility est ignorée (élagage du modèle).
#'
#' @param p_facilities integer. Nombre de facilities à ouvrir.
#' @param solver character. Solveur ROI : \code{"glpk"} (défaut)


# demand : sf de points en 4326 (obligatoire)
# demand_id : champ unique id de type char  (obligatoire)
# demand_weight : champ de type numeric (optionel), sinon NULL mettre 1

# existing_sites (optionel)
# existing_sites_id : c(obligatoire, si existing_sites est là)
# existing_sites_weight : champ de type numeric (optionel), sinon NULL mettre 1

# candidate : sf de points en 4326 (obligatoire)
# candidate_id : champ unique id de type char  (obligatoire)
# candidate_weight : champ de type numeric (optionel), sinon NULL mettre 1

# matrix_OD : data.frame (obligatoire)
# matrix_OD_existing_site : (optionnel)
#' @return Une liste de classe \code{localalloc_result}.
#' @export
p_median <- function(candidate,
                     candidate_id,
                     candidate_weight             = NULL,

                     existing_sites               = NULL,
                     existing_sites_id            = NULL,
                     existing_sites_weight        = NULL,

                     demand,
                     demand_id,
                     demand_weight                = NULL,

                     matrix_OD_candidates,
                     matrix_OD_candidates_from_id = "from_id",
                     matrix_OD_candidates_to_id   = "to_id",
                     matrix_OD_candidates_dist    = "distance",

                     matrix_OD_existing_site          = NULL,
                     matrix_OD_existing_site_from_id  = NULL,
                     matrix_OD_existing_site_to_id    = NULL,
                     matrix_OD_existing_site_dist     = NULL,

                     cutoff_distance = 1000,
                     p_facilities,
                     solver          = "glpk") {
  # ***********************************************************
  # 1. Validation ---------------------------------------------
  # ***********************************************************
  # 1.1 Validation objets sf ----------------------------------
  .validate_sf(candidate, "candidate", candidate_id)
  .validate_sf(demand, "demand", demand_id)

  # 1.2 Validation des existing_sites -------------------------
  has_existing <- !is.null(existing_sites)
  if (has_existing) {
    if (is.null(existing_sites_id))
      stop("`existing_sites_id` est obligatoire si `existing_sites` est fourni.")
    .validate_sf(existing_sites, "existing_sites", existing_sites_id)
    if (is.null(matrix_OD_existing_site))
      stop("`matrix_OD_existing_site` est obligatoire si `existing_sites` est fourni.")
  }

  # Vérifier qu'il n'y a pas de collision d'identifiants entre
  # candidates et existing_sites (sinon la fusion des matrices
  # confondrait deux facilities différentes)
  collision <- intersect(
    as.character(candidate[[candidate_id]]),
    as.character(existing_sites[[existing_sites_id]])
  )
  if (length(collision) > 0)
    stop(sprintf(
      "Des identifiants sont partagés entre `candidate` et `existing_sites` : %s",
      paste(collision, collapse = ", ")
    ))

  # 1.3 Validation numérique ----------------------------------
  if (!is.numeric(cutoff_distance) || cutoff_distance <= 0)
    stop("`cutoff_distance` doit être un nombre positif.")

  if (!is.numeric(p_facilities) || p_facilities < 1)
    stop("`p_facilities` doit être un entier >= 1.")
  p_facilities <- as.integer(p_facilities)


  # 1.4 Validation des matrices -------------------------------
  .validate_cost_matrix(
    matrix_OD_candidates,
    matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id,
    matrix_OD_candidates_dist,
    nom = "matrix_OD_candidates"
  )

  if (has_existing) {
    .validate_cost_matrix(
      matrix_OD_existing_site,
      matrix_OD_existing_site_from_id,
      matrix_OD_existing_site_to_id,
      matrix_OD_existing_site_dist,
      nom = "matrix_OD_existing_site"
    )
  }


  # ***********************************************************
  # 2. Préparation des données  -------------------------------
  # ***********************************************************
  # 2.1 Identifiants ------------------------------------------------
  ids_demand <- as.character(demand[[demand_id]])
  ids_cand   <- as.character(candidate[[candidate_id]])
  n_cli      <- length(ids_demand)
  n_fac      <- length(ids_cand)
  if (p_facilities > n_fac)
    stop(sprintf(
      "`p_facilities` (%d) ne peut pas dépasser le nombre de candidates (%d).",
      p_facilities, n_fac
    ))

  if (has_existing) {
    ids_exist <- as.character(existing_sites[[existing_sites_id]])
    n_exist   <- length(ids_exist)
  }

  # 2.2 Poids -------------------------------------------------
  demand <- .set_weights(demand, demand_id, demand_weight, "demand")
  candidate <- .set_weights(candidate, candidate_id, candidate_weight, "candidate")
  if (has_existing)
    existing_sites <- .set_weights(existing_sites,
                                   existing_sites_id,
                                   existing_sites_weight,
                                   "existing_sites")
  col_weight_demand <- if (is.null(demand_weight)) "weight" else demand_weight
  weight_demand      <- as.numeric(demand[[col_weight_demand]])
  # 2.3 Matrices de coûts individuelles -----------------------
  # Candidates : n_cli x n_fac
  cost_mat_cand <- .od_to_matrix(
    matrix_OD_candidates,
    matrix_OD_candidates_from_id,
    matrix_OD_candidates_to_id,
    matrix_OD_candidates_dist,
    cutoff_distance
  )
  # Réordonner lignes/colonnes pour garantir l'alignement avec ids_demand/ids_cand
  cost_mat_cand <- cost_mat_cand[ids_demand, ids_cand, drop = FALSE]
  .warn_unreachable(cost_mat_cand, ids_demand, "candidate")

  if (has_existing) {
    cost_mat_exist <- .od_to_matrix(
      matrix_OD_existing_site,
      matrix_OD_existing_site_from_id,
      matrix_OD_existing_site_to_id,
      matrix_OD_existing_site_dist,
      cutoff_distance
    )
    cost_mat_exist <- cost_mat_exist[ids_demand, ids_exist, drop = FALSE]
  }
  # 2.4 Matrice fusionnée pour le ILP --------------------------------
  # Colonnes 1:n_fac         = candidates
  # Colonnes (n_fac+1):n_all = existing (Required Facilities)
  if (has_existing) {
    ids_all_fac  <- c(ids_cand, ids_exist)
    cost_mat_all <- cbind(cost_mat_cand, cost_mat_exist)
  } else {
    ids_all_fac  <- ids_cand
    cost_mat_all <- cost_mat_cand
  }
  n_all_fac <- length(ids_all_fac)

  # Remplacer Inf par une grande valeur pour la compatibilité solveur
  cost_mat <- .replace_inf(cost_mat_all)

  # ***********************************************************
  # 3. Formulation ILP avec ompr  -----------------------------
  # ***********************************************************
  # Contraintes :
  #   Σ_{j=1}^{n_fac}     X[j]    = p_facilities   (budget candidates)
  #   X[j] = 1      ∀ j > n_fac  (existing = Required)
  #   Σ_j Y[i,j] = 1    ∀i       (chaque client assigné à 1 facility)
  #   Y[i,j] <= X[j] ∀i,j      (client assigné seulement si facility ouverte)
  #
  # Variable binaire :
  #   X[j] = 1 si la facility j est ouverte, 0 sinon
  #   Y[i,j]  = 1 si client i est assigné à facility j
  #
  # Objectif (minimisation) :
  #   min  Σ_i Σ_j  weight_demand[i] * cost_mat[i,j] * Y[i,j]
  #

  # Création modèle avec les variable, contrainte et couverture
  model <- ompr::MIPModel() |>

    # X[j] pour toutes les facilities (candidates + existing)
    ompr::add_variable(X[j], j    = 1:n_all_fac, type = "binary") |>

    # Assignation client-facility : Y[i,j]
    ompr::add_variable(Y[i, j],
                       i    = 1:n_cli,
                       j    = 1:n_all_fac,
                       type = "binary") |>

    # Objectif : minimiser la distance totale pondérée
    ompr::set_objective(ompr::sum_expr(
      weight_demand[i] * cost_mat[i, j] * Y[i, j],
      i = 1:n_cli,
      j = 1:n_all_fac
    ),
    sense = "min") |>

    # Contrainte budget
    ompr::add_constraint(ompr::sum_expr(X[j], j = 1:n_fac) == p_facilities) |>

    # Chaque client est assigné à exactement 1 facility
    ompr::add_constraint(ompr::sum_expr(Y[i, j], j = 1:n_all_fac) == 1, i = 1:n_cli) |>

    # Un client ne peut être assigné qu'à une facility ouverte
    ompr::add_constraint(Y[i, j] <= X[j], i = 1:n_cli, j = 1:n_all_fac)

  # Required Facilities : forcer tous les existing ouverts
  if (has_existing) {
    model <- ompr::add_constraint(
      model,
      X[j] == 1,
      j = (n_fac + 1):n_all_fac
    )
  }

  # ***********************************************************
  # 4. Résolution ---------------------------------------------
  # ***********************************************************
  message(sprintf(
    "P-Median | %d demandes | %d candidates (p=%d)%s | solveur : %s",
    n_cli, n_fac, p_facilities,
    if (has_existing) sprintf(" | %d existing (forcés)", n_exist) else "",
    solver
  ))

  result <- tryCatch(
    ompr::solve_model(model, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop(sprintf(
      "Erreur du solveur '%s' : %s\nVérifie que ROI.plugin.%s est installé.",
      solver, e$message, solver
    ))
  )

  if (result$status != "optimal")
    warning(sprintf("Solution non optimale. Statut : '%s'", result$status))
  # ***********************************************************
  # 5. Extraction de la solution ------------------------------
  # ***********************************************************
  # Vecteur binaire : X_vals[j] = 1 si facility j est ouverte
  X_vals <- ompr::get_solution(result, X[j])$value
  Y_vals <- ompr::get_solution(result, Y[i, j])

  selected_j   <- which(round(X_vals[1:n_fac]) == 1)
  ids_selected <- ids_cand[selected_j]

  # Table d'assignation demande -> facility (candidate ou existing)
  assignments <- .extract_assignment(
    Y_vals   = Y_vals,
    ids_from = ids_demand,
    ids_to   = ids_all_fac,
    cost_mat = cost_mat_all
  )
  assignments$source <- ifelse(
    assignments$facility_id %in% ids_cand, "candidate", "existing"
  )

  total_cost <- sum(
    poids_demand * ifelse(is.finite(assignments$distance), assignments$distance, 0),
    na.rm = TRUE
  )

  # ***********************************************************
  # 6.Construction du fichier de points et retour résultat ----
  # ***********************************************************
  sf_results <- .build_result_sf(
    candidate      = candidate,
    candidate_id   = candidate_id,
    ids_selected   = ids_selected)

  structure(
    list(
      model_type    = "p_median",
      solver_status = result$status,

      # Objets sf — visualisation directe
      sf_selected   = sf_results$sf_selected,

      # Statistiques
      total_cost    = total_cost,
      n_open        = length(ids_selected) + if (has_existing) n_exist else 0L,
      n_demand      = n_cli,
    ),
    class = "localalloc_result"
  )
}
