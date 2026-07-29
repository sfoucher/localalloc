# ***********************************************************
# utilitaires.R
# Fonctions internes partagées par les 4 modèles
# ***********************************************************

# ***********************************************************
# .validate_cost_matrix
# Valide qu'un data.frame OD long est bien formé
# ***********************************************************
# cost_matrix : data.frame long (from_id, to_id, distance)
# col_id      : nom de la colonne origine
# col_to_id   : nom de la colonne destination
# col_dist    : nom de la colonne distance
# nom         : nom du paramètre (pour messages d'erreur lisibles)
.validate_cost_matrix <- function(cost_matrix, col_id, col_to_id, col_dist,
                                  nom = "cost_matrix") {

  if (!is.data.frame(cost_matrix))
    stop(sprintf("`%s` doit être un data.frame.", nom))

  list_id <- list(col_id, col_to_id, col_dist)
  for (id in list_id) {
    if (is.null(id))
      stop(sprintf("Un nom de colonne obligatoire est NULL dans `%s`.", nom))

    if (!id %in% names(cost_matrix))
      stop(sprintf("La colonne '%s' n'existe pas dans `%s`.", id, nom))

    ids <- cost_matrix[[id]]
    if (any(is.na(ids)))
      stop(sprintf("La colonne '%s' de `%s` contient des valeurs NA.", id, nom))
  }

  if (any(cost_matrix[[col_dist]] < 0))
    stop(sprintf(
      "La colonne '%s' de `%s` contient des distances négatives.", col_dist, nom
    ))

  if (nrow(cost_matrix) < 1 || ncol(cost_matrix) < 1)
    stop(sprintf("`%s` doit avoir au moins 1 ligne et 1 colonne.", nom))
}
# ***********************************************************
# .validate_sf
# Valide qu'un objet est bien un sf avec une colonne id valide
# ***********************************************************
# obj      : objet à valider
# nom      : nom du paramètre (pour le message d'erreur)
# col_id   : nom de la colonne identifiant
.validate_sf <- function(obj, nom, col_id) {
  if (!inherits(obj, "sf"))
    stop(sprintf("`%s` doit être un objet sf.", nom))
  if (!is.null(col_id)) {
    if (!col_id %in% names(obj))
      stop(sprintf("La colonne '%s' n'existe pas dans `%s`.", col_id, nom))
    ids <- obj[[col_id]]
    if (any(is.na(ids)))
      stop(sprintf("La colonne '%s' de `%s` contient des valeurs NA.", col_id, nom))
    if (anyDuplicated(ids))
      stop(sprintf(
        "La colonne '%s' de `%s` contient des identifiants dupliqués.",
        col_id,
        nom
      ))
  }
  else
    stop(sprintf("L'identifiant `%s` can't be NA.", col_id))
}
# ***********************************************************
# .od_to_matrix
# Convertit un data.frame OD long en matrice numérique
# n_cli x n_fac avec Inf pour les paires absentes ou
# au-delà du cutoff
# ***********************************************************
# od            : data.frame
# id_from       : identifiants lignes (clients)
# id_to         : identifiants colonnes (facilities)
# dist          : identifiant colonne distance
# cutoff        : distance max
.od_to_matrix <- function(od, id_from, id_to, dist, cutoff = 1000) {

  ids_from <- unique(od[[id_from]])
  ids_to   <- unique(od[[id_to]])

  # Matrice initialisée à Inf — toute paire non renseignée reste Inf
  mat <- matrix(
    Inf,
    nrow     = length(ids_from),
    ncol     = length(ids_to),
    dimnames = list(ids_from, ids_to)
  )

  # Ne garder que les paires sous le cutoff
  od_valide <- od[od[[dist]] <= cutoff, ]

  # Remplir la matrice à partir du format long
  idx_from <- match(od_valide[[id_from]], ids_from)
  idx_to   <- match(od_valide[[id_to]],   ids_to)
  mat[cbind(idx_from, idx_to)] <- od_valide[[dist]]

  mat
}
# ***********************************************************
# .replace_inf
# Remplace les Inf d'une matrice par une grande valeur M
# pour la compatibilité avec les solveurs ILP
# ***********************************************************
# mat : matrix numérique (peut contenir des Inf)
# M   : valeur de remplacement (défaut : 10 x max fini)
.replace_inf <- function(mat, M = NULL) {
  if (is.null(M)) {
    valeurs_finies <- mat[is.finite(mat)]
    M <- if (length(valeurs_finies) > 0) max(valeurs_finies) * 10 else 1e9
  }
  mat[is.infinite(mat)] <- M
  mat
}
# ***********************************************************
# .warn_unreachable
# Avertit si des lignes (clients) n'ont aucune colonne
# (facility) accessible dans la matrice
# ***********************************************************
# cost_mat : matrix numérique n_from x n_to (avec Inf)
# ids_from : identifiants des lignes
# contexte : label pour le message
.warn_unreachable <- function(cost_mat, ids_from, contexte = "facility") {
  inaccessibles <- which(apply(cost_mat, 1, function(r) all(is.infinite(r))))
  if (length(inaccessibles) > 0)
    warning(sprintf(
      "%d point(s) de demande sans %s accessible : [%s]",
      length(inaccessibles), contexte,
      paste(ids_from[inaccessibles], collapse = ", ")
    ))
}

# ***********************************************************
# .set_weights
# Remplace les valeurs NULL/NA dans la colonne de poids
# d'un objet sf par 1, et crée la colonne si elle n'existe pas.
# Retourne le sf mis à jour.
# ***********************************************************
# sf_obj   : objet sf à modifier
# col_id   : colonne identifiant (pour les messages d'erreur)
# col_w    : nom de la colonne poids (NULL = créer "weight" avec valeur 1)
# nom      : nom du paramètre pour les messages d'erreur
.set_weights <- function(sf_obj,
                         col_id,
                         col_w = NULL,
                         nom = "objet") {
  # Si aucune colonne fournie → créer une colonne "weight" à 1
  if (is.null(col_w)) {
    sf_obj[["weight"]] <- 1L
    warning(
      sprintf(
        "Le poids de `%s` n'a pas été fourni donc une  colonne weight à 1 a
      été créée.",
        col_w
      )
    )
    return(sf_obj)
  }

  # Vérifier que la colonne existe
  if (!col_w %in% names(sf_obj))
    stop(sprintf("La colonne de poids '%s' n'existe pas dans `%s`.", col_w, nom))

  # Vérifier que la colonne est numérique
  if (!is.numeric(sf_obj[[col_w]]))
    stop(sprintf("La colonne '%s' de `%s` doit être numérique.", col_w, nom))

  # Vérifier les valeurs négatives
  if (any(sf_obj[[col_w]] < 0, na.rm = TRUE))
    stop(sprintf(
      "La colonne '%s' de `%s` contient des valeurs négatives.",
      col_w,
      nom
    ))

  # Remplacer les NA par 1
  n_na <- sum(is.na(sf_obj[[col_w]]))
  if (n_na > 0) {
    warning(
      sprintf(
        "%d valeur(s) NA dans la colonne '%s' de `%s` remplacée(s) par 1.",
        n_na,
        col_w,
        nom
      )
    )
    sf_obj[[col_w]][is.na(sf_obj[[col_w]])] <- 1L
  }

  sf_obj
}

# ***********************************************************
# .make_coverage_matrix
# Convertit une matrice numérique n_cli x n_fac en matrice
# binaire de couverture (1 si distance <= service_radius)
# ***********************************************************
# cost_matrix    : matrix numérique n_cli x n_fac
# service_radius : seuil de couverture
.make_coverage_matrix <- function(cost_matrix, service_radius) {
  bij <- (cost_matrix <= service_radius) * 1L # 1L est un entier
  storage.mode(bij) <- "integer"
  bij
}
# ***********************************************************
# .extract_assignment
# Extrait l'assignation client -> facility depuis la solution
# Y[i,j] retournée par ompr::get_solution()
# Retourne un data.frame avec demand_id, facility_id, distance
# ***********************************************************
# Y_vals   : data.frame retourné par ompr::get_solution(result, Y[i,j])
# ids_from : vecteur d'identifiants (indexé par i)
# ids_to   : vecteur d'identifiants facilities (indexé par j)
# cost_mat : matrix originale
.extract_assignment <- function(Y_vals, ids_from, ids_to, cost_mat) {
  n_cli <- length(ids_from)

  demand_id   <- character(n_cli)
  facility_id <- character(n_cli)
  distance    <- numeric(n_cli)

  for (i in seq_len(n_cli)) {
    demand_id[i] <- ids_from[i]
    row_i <- Y_vals[Y_vals$i == i & round(Y_vals$value) == 1, ]

    if (nrow(row_i) == 0) {
      facility_id[i] <- NA_character_
      distance[i]    <- NA_real_
    } else {
      j <- row_i$j[1]
      facility_id[i] <- ids_to[j]
      distance[i]    <- cost_mat[i, j]
    }
  }

  data.frame(
    demand_id   = demand_id,
    facility_id = facility_id,
    distance    = distance
  )
}

# ***********************************************************
# .build_result_sf
# Reconstruit les objets sf de la solution pour visualisation
# ***********************************************************
# candidate      : sf original des candidates
# candidate_id   : colonne id dans candidate
# ids_selected   : vecteur des ids candidates sélectionnées (j <= n_fac)
.build_result_sf <- function(candidate,
                             candidate_id,
                             ids_selected) {

  # --- sf candidates sélectionnées ----------------------------
  sf_selected <- candidate[
    as.character(candidate[[candidate_id]]) %in% ids_selected, ,
    drop = FALSE
  ]

  n_assigned_cand <- vapply(ids_selected, function(fid) {
    sum(!is.na(assignments$facility_id) &
          assignments$facility_id == fid &
          assignments$source == "candidate")
  }, integer(1))

  sf_selected$n_assigned <- n_assigned_cand[
    match(as.character(sf_selected[[candidate_id]]), ids_selected)
  ]
  sf_selected$facility_type <- "candidate"



  list(
    sf_selected = sf_selected
  )
}
# ***********************************************************
# print.localalloc_result
# Méthode d'affichage commune aux 4 modèles
# ***********************************************************
#' @export
print.localalloc_result <- function(x, ...) {
  cat("\n")
  cat("=======================================================\n")
  cat(sprintf("  Modèle        : %s\n", toupper(x$model_type)))
  cat(sprintf("  Statut        : %s\n", x$solver_status))
  cat("-------------------------------------------------------\n")
  cat(sprintf("  Candidates    : %d ouverte(s) (p = %d)\n",
              nrow(x$sf_selected), x$p_facilities))
  if (!is.null(x$total_cost))
    cat(sprintf("  Coût total    : %.2f\n", x$total_cost))
  cat("=======================================================\n\n")
  invisible(x)
}
