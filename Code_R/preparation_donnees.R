# ==============================================================================
# PROJET D'ÉTUDE STATISTIQUE SUR LES ACCIDENTS CORPORELS (FRANCE - 2024)
# PIPELINE ETL : NETTOYAGE, IMPUTATIONS MÉTIER ET ENRICHISSEMENT DES DONNÉES
# ==============================================================================

# Se positionner sur la racine du projet si exécuté depuis Code_R
if (basename(getwd()) == "Code_R") {
  setwd("..")
}

# ------------------------------------------------------------------------------
# Librairies requises pour l'ETL
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# ==============================================================================
# ÉTAPE 1 : CHARGEMENT DES FICHIERS SOURCES BRUTS ET POPULATION INSEE
# ==============================================================================
cat("--> [1/6] Chargement des fichiers bruts BAAC et INSEE...\n")

caracteristiques_2024 <- read.csv("Ressources/caract-2024.csv", sep = ";", header = TRUE)
lieux_2024            <- read.csv("Ressources/lieux-2024.csv", sep = ";", header = TRUE)
vehicules_2024        <- read.csv("Ressources/vehicules-2024.csv", sep = ";", header = TRUE)
usagers_2024          <- read.csv("Ressources/usagers-2024.csv", sep = ";", header = TRUE)

# Nettoyage et standardisation de la population municipale INSEE par département
population <- read.csv("Ressources/Insee.csv") %>%
  select(
    dep        = Code.département,
    nom_dep    = Nom.du.département,
    population = Population.municipale
  ) %>%
  mutate(
    dep        = trimws(as.character(dep)),
    population = as.numeric(gsub("[, ]", "", population))
  )

# ==============================================================================
# ÉTAPE 2 : VARIABLE CIBLE ET FILTRAGE DE COHÉRENCE
# ==============================================================================
cat("--> [2/6] Calcul de la variable cible (Nb_victimes non indemnes)...\n")

# Variable cible : somme des usagers blessés légers, hospitalisés ou tués (grav != 1)
nb_victimes_par_accident <- usagers_2024 %>%
  group_by(Num_Acc) %>%
  filter(grav != 1) %>% # grav 1 = indemne
  summarise(Nb_victimes = n(), .groups = "drop")

accidents_2024 <- caracteristiques_2024 %>%
  left_join(nb_victimes_par_accident, by = "Num_Acc") %>%
  mutate(Nb_victimes = ifelse(is.na(Nb_victimes), 0, Nb_victimes))

# Suppression de la colonne adresse non exploitable statistiquement
accidents_2024$adr <- NULL

# Suppression des 6 accidents sans type de collision valide (col == -1)
accidents_2024 <- accidents_2024 %>%
  filter(col != -1)

# ==============================================================================
# ÉTAPE 3 : VARIABLES TEMPORELLES ET LABELS CATÉGORIELS
# ==============================================================================
cat("--> [3/6] Standardisation temporelle et labels descriptifs...\n")

# 1. Saisonnalité ordonnée chronologiquement
accidents_2024 <- accidents_2024 %>%
  mutate(
    saison = case_when(
      mois %in% c(12, 1, 2)  ~ "Hiver",
      mois %in% c(3, 4, 5)   ~ "Printemps",
      mois %in% c(6, 7, 8)   ~ "Ete",
      mois %in% c(9, 10, 11) ~ "Automne"
    ),
    saison = factor(saison, levels = c("Hiver", "Printemps", "Ete", "Automne")),
    atm    = as.factor(atm)
  )

# 2. Encodage numérique du département
accidents_2024$dep_code <- as.integer(as.factor(accidents_2024$dep))

# 3. Date complète et jour de la semaine en français
accidents_2024$date <- as.Date(
  paste(accidents_2024$an, accidents_2024$mois, accidents_2024$jour, sep = "-")
)

accidents_2024$jour_semaine <- factor(
  weekdays(accidents_2024$date),
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
  labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
)

# 4. Heure numérique (0 à 23)
accidents_2024$heure <- as.numeric(substr(accidents_2024$hrmn, 1, 2))
accidents_2024$hrmn  <- NULL

# 5. Jointure population INSEE
accidents_2024 <- accidents_2024 %>% left_join(population, by = "dep")

# 6. Dictionnaires de labels pour l'analyse
lab_atm <- c("1" = "Normale", "2" = "Pluie légère", "3" = "Pluie forte",
             "4" = "Neige/Grêle", "5" = "Brouillard", "6" = "Vent fort",
             "7" = "Temps éblouissant", "8" = "Temps nuageux", "9" = "Autre")

lab_lum <- c("1" = "Plein jour", "2" = "Crépuscule/aube",
             "3" = "Nuit sans éclairage", "4" = "Nuit avec éclairage",
             "5" = "Nuit éclairage allumé")

lab_col <- c("1" = "Deux véh. - frontale",
             "2" = "Deux véh. - par l'arrière",
             "3" = "Deux véh. - par le côté",
             "4" = "Trois véh.+ en chaîne",
             "5" = "Trois véh.+ conf. multiple",
             "6" = "Autre collision",
             "7" = "Sans collision")

lab_int <- c("1" = "Hors intersection", "2" = "En X", "3" = "En T",
             "4" = "En Y", "5" = "> 4 branches", "6" = "Rond-point",
             "7" = "Place", "8" = "Passage à niveau", "9" = "Autre")

accidents_2024$atm_label <- factor(lab_atm[as.character(accidents_2024$atm)], levels = lab_atm)
accidents_2024$lum_label <- factor(lab_lum[as.character(accidents_2024$lum)], levels = lab_lum)
accidents_2024$agg_label <- factor(accidents_2024$agg, levels = 1:2, labels = c("Hors agglomération", "En agglomération"))

# Typologie de collision en 6 catégories homogènes (validé empiriquement) :
accidents_2024$col_regroup <- factor(
  case_when(
    accidents_2024$col == 1     ~ "Deux véh. - Frontale",
    accidents_2024$col %in% 2:3 ~ "Deux véh. - Arrière / Côté",
    accidents_2024$col == 4     ~ "Trois véh.+ en chaîne",
    accidents_2024$col == 5     ~ "Trois véh.+ coll. multiples",
    accidents_2024$col == 6     ~ "Autre collision",
    accidents_2024$col == 7     ~ "Sans collision"
  ),
  levels = c(
    "Deux véh. - Frontale",
    "Deux véh. - Arrière / Côté",
    "Trois véh.+ en chaîne",
    "Trois véh.+ coll. multiples",
    "Autre collision",
    "Sans collision"
  )
)

# ==============================================================================
# ÉTAPE 4 : IMPUTATION ET REGROUPEMENT DE L'INFRASTRUCTURE (VMA ET SURFACE)
# ==============================================================================
cat("--> [4/6] Imputation de la VMA et de l'état de la chaussée...\n")

lieux_acc <- lieux_2024 %>%
  select(Num_Acc, vma, surf) %>%
  left_join(caracteristiques_2024 %>% select(Num_Acc, agg, atm), by = "Num_Acc") %>%
  mutate(
    # 1. Correction et imputation métier de la VMA
    vma = ifelse(vma %in% c(300, 500, 700, 800, 900), vma / 10, vma),
    vma = ifelse(vma %in% 1:9, vma * 10, vma), # Correction des oublis de zéro (5 -> 50, 8 -> 80)
    vma = ifelse(vma < 10 | vma > 130, NA, vma),
    vma = case_when(
      !is.na(vma) ~ vma,
      agg == 2    ~ 50, # 50 km/h en ville
      agg == 1    ~ 80, # 80 km/h hors agglomération
      TRUE        ~ 50
    ),
    # 2. Imputation contextuelle de la surface (croisement avec la météo atm)
    surf_impute = case_when(
      surf %in% 1:9    ~ surf,
      atm %in% c(2, 3) ~ 2, # Pluie -> chaussée mouillée
      atm == 4         ~ 5, # Neige/grêle -> enneigée
      atm == 5         ~ 2, # Brouillard -> humide/mouillée
      TRUE             ~ 1  # Normale / sèche par défaut
    ),
    # 3. Indicatrice binaire de chaussée glissante / dégradée
    chaussee_glissante = as.integer(surf_impute %in% 2:8)
  ) %>%
  # Dédoublonnage des intersections : situation la plus défavorable
  group_by(Num_Acc) %>%
  summarise(
    vma                = max(vma),
    surf               = max(surf_impute),
    chaussee_glissante = as.integer(any(chaussee_glissante == 1)),
    .groups            = "drop"
  ) %>%
  mutate(
    # Regroupement en 3 catégories homogènes
    surf_regroup = factor(
      case_when(
        surf == 1     ~ "Normale",
        surf %in% 2:4 ~ "Mouillee / Inondee",
        surf %in% 5:8 ~ "Glissante critique",
        TRUE          ~ "Autre"
      ),
      levels = c("Normale", "Mouillee / Inondee", "Glissante critique", "Autre")
    )
  )

# ==============================================================================
# ÉTAPE 5 : ENRICHISSEMENT DEPUIS LES TABLES VÉHICULES ET USAGERS
# ==============================================================================
cat("--> [5/6] Agrégation par accident des véhicules, usagers, âge et obstacles...\n")

# 1. Typologies de véhicules et obstacles fixes
vehic_acc <- vehicules_2024 %>%
  mutate(obs_impute = ifelse(obs %in% 0:17, obs, 0)) %>% # Imputation par le mode (0 = sans obstacle fixe)
  group_by(Num_Acc) %>%
  summarise(
    nb_vehicules_impliquees = n(),
    implique_velo_edp      = as.integer(any(catv %in% c(1, 50, 60, 80))),
    implique_voiture       = as.integer(any(catv %in% c(3, 7, 8, 9, 10, 11, 12))),
    implique_moto          = as.integer(any(catv %in% c(2, 4, 5, 6, 30, 31, 32, 33, 34, 35, 36, 41, 42, 43))),
    implique_poids_lourd   = as.integer(any(catv %in% c(13, 14, 15, 16, 17, 20, 21))),
    implique_transp_commun = as.integer(any(catv %in% c(18, 19, 37, 38, 39, 40))),
    # Chocs contre obstacle fixe
    choc_obstacle_fixe     = as.integer(any(obs_impute %in% 1:17)),
    choc_arbre_mur         = as.integer(any(obs_impute %in% c(2, 6, 13))), # Choc lourd
    .groups                = "drop"
  )

# 2. Usagers, âge, sexe et équipements de sécurité
usagers_acc <- usagers_2024 %>%
  mutate(
    # Nettoyage et imputation conditionnelle de l'année de naissance
    an_nais_clean = ifelse(an_nais >= 1910 & an_nais <= 2024, an_nais, NA),
    an_nais_impute = case_when(
      !is.na(an_nais_clean) ~ an_nais_clean,
      catu == 1             ~ 1983, # Conducteur : médiane 41 ans
      catu == 2             ~ 2000, # Passager : médiane 24 ans
      catu == 3             ~ 1986, # Piéton : médiane 38 ans
      TRUE                  ~ 1985
    ),
    age = 2024 - an_nais_impute,
    # Imputation du sexe par le mode (1 = Homme)
    sexe_impute = ifelse(sexe %in% 1:2, sexe, 1)
  ) %>%
  group_by(Num_Acc) %>%
  summarise(
    implique_pieton        = as.integer(any(catu == 3)),
    defaut_securite        = as.integer(any(secu1 == 0)), # Non port ceinture / casque
    # Profils démographiques usagers
    implique_jeune_18_24   = as.integer(any(age >= 18 & age <= 24)),
    implique_senior_65plus = as.integer(any(age >= 65)),
    conducteur_homme       = as.integer(any(catu == 1 & sexe_impute == 1)),
    age_moyen_usagers      = round(mean(age), 1),
    .groups                = "drop"
  )

# ==============================================================================
# ÉTAPE 6 : CONSOLIDATION FINALE, CORRECTION GPS ET EXPORT
# ==============================================================================
cat("--> [6/6] Consolidation, correction des coordonnées GPS et export...\n")

accidents_2024 <- accidents_2024 %>%
  left_join(lieux_acc, by = "Num_Acc") %>%
  left_join(vehic_acc, by = "Num_Acc") %>%
  left_join(usagers_acc, by = "Num_Acc") %>%
  mutate(
    nb_vehicules_impliquees = coalesce(nb_vehicules_impliquees, 1L),
    implique_velo_edp       = coalesce(implique_velo_edp, 0L),
    implique_voiture        = coalesce(implique_voiture, 0L),
    implique_moto           = coalesce(implique_moto, 0L),
    implique_poids_lourd    = coalesce(implique_poids_lourd, 0L),
    implique_transp_commun  = coalesce(implique_transp_commun, 0L),
    implique_pieton         = coalesce(implique_pieton, 0L),
    defaut_securite         = coalesce(defaut_securite, 0L),
    choc_obstacle_fixe      = coalesce(choc_obstacle_fixe, 0L),
    choc_arbre_mur          = coalesce(choc_arbre_mur, 0L),
    implique_jeune_18_24    = coalesce(implique_jeune_18_24, 0L),
    implique_senior_65plus  = coalesce(implique_senior_65plus, 0L),
    conducteur_homme        = coalesce(conducteur_homme, 0L),
    chaussee_glissante      = coalesce(chaussee_glissante, 0L)
  )

# Conversion numérique des coordonnées
accidents_2024 <- accidents_2024 %>%
  mutate(
    lat          = as.numeric(sub(",", ".", lat)),
    long         = as.numeric(sub(",", ".", long)),
    is_metropole = !grepl("^9[78]", dep)
  )

# Correction des inversions de coordonnées (lat/long interverties dans le BAAC)
# En métropole : lat ~ 41-52, long ~ -5.5 à 10 (ex. Haute-Corse 2B)
# À Mayotte (976) : lat ~ -12.8, long ~ 45.2
accidents_2024 <- accidents_2024 %>%
  mutate(
    inversion_metropole = is_metropole & (long >= 41 & long <= 52 & lat >= -5.5 & lat <= 10),
    inversion_mayotte   = (dep == "976") & (lat > 40 & abs(long) < 20),
    
    lat_corr = case_when(
      inversion_metropole ~ long,
      inversion_mayotte   ~ -abs(long),
      TRUE                ~ lat
    ),
    long_corr = case_when(
      inversion_metropole ~ lat,
      inversion_mayotte   ~ abs(lat),
      TRUE                ~ long
    ),
    lat  = lat_corr,
    long = long_corr
  ) %>%
  select(-inversion_metropole, -inversion_mayotte, -lat_corr, -long_corr)

# Imputation par le barycentre départemental pour coordonnées manquantes ou aberrantes
accidents_2024 <- accidents_2024 %>%
  group_by(dep) %>%
  mutate(
    coord_invalide = (is_metropole & (is.na(lat) | is.na(long) | abs(lat) < 1e-4 | abs(long) < 1e-4 |
                                       lat < 41 | lat > 52 | long < -5.5 | long > 10)) |
                     (!is_metropole & (is.na(lat) | is.na(long) | abs(lat) < 1e-4 | abs(long) < 1e-4)),
    lat  = if_else(coord_invalide, mean(lat[!coord_invalide], na.rm = TRUE), lat),
    long = if_else(coord_invalide, mean(long[!coord_invalide], na.rm = TRUE), long)
  ) %>%
  ungroup() %>%
  select(-coord_invalide)



# Harmonisation des libellés de département pour l'Outre-mer sans recensement standard
accidents_2024 <- accidents_2024 %>%
  mutate(nom_dep = if_else(is.na(nom_dep) & grepl("^9[78]", dep), "Outre-Mer (COM)", nom_dep))

# Sélection des variables d'intérêt pour la modélisation et l'analyse
accidents_modele <- accidents_2024 %>%
  select(
    # 1. Identifiants & Géographie
    Num_Acc, dep, nom_dep, lat, long, is_metropole, population,
    
    # 2. Variable Cible (Sévérité)
    Nb_victimes,
    
    # 3. Temporel
    date, mois, saison, jour_semaine, heure,
    
    # 4. Environnement & Météo
    lum_label, atm_label, surf_regroup, chaussee_glissante,
    
    # 5. Infrastructure & Choc
    agg_label, int, vma, col_regroup, choc_obstacle_fixe, choc_arbre_mur,
    
    # 6. Véhicules impliqués
    nb_vehicules_impliquees, implique_voiture, implique_moto,
    implique_velo_edp, implique_poids_lourd, implique_transp_commun,
    
    # 7. Usagers & Facteurs humains
    implique_pieton, defaut_securite, implique_jeune_18_24,
    implique_senior_65plus, conducteur_homme, age_moyen_usagers
  )


# ==============================================================================
# AUDIT DE CONFORMITÉ ET INTÉGRITÉ DES DONNÉES
# ==============================================================================
cat("\n=======================================================\n")
cat("      RAPPORT D'AUDIT QUALITÉ DES DONNÉES 2024         \n")
cat("=======================================================\n")
cat(sprintf("• Observations retenues : %d accidents\n", nrow(accidents_modele)))
cat(sprintf("• Variables finales     : %d colonnes\n", ncol(accidents_modele)))

# Test des valeurs manquantes (NA)
na_metropole <- sum(is.na(accidents_modele %>% filter(is_metropole)))
cat(sprintf("• Valeurs manquantes en métropole (N = %d) : %d\n", 
            sum(accidents_modele$is_metropole), na_metropole))

# Test des codes d'absence (-1)
nb_minus_one <- sum(sapply(accidents_modele, function(x) if (is.numeric(x)) sum(x == -1, na.rm = TRUE) else 0))
cat(sprintf("• Codes d'absence non imputés (-1) : %d\n", nb_minus_one))

# Test de cohérence des plages de valeurs
test_coherence <- list(
  "VMA autorisée (10 à 130 km/h)"       = sum(accidents_modele$vma < 10 | accidents_modele$vma > 130),
  "Heure de l'accident (0 à 23h)"       = sum(accidents_modele$heure < 0 | accidents_modele$heure > 23),
  "Mois de l'accident (1 à 12)"         = sum(accidents_modele$mois < 1 | accidents_modele$mois > 12),
  "Nombre de victimes (>= 1)"           = sum(accidents_modele$Nb_victimes < 1),
  "GPS métropolitain valide"            = sum(accidents_modele$is_metropole & 
                                                (accidents_modele$lat < 41 | accidents_modele$lat > 52 | 
                                                 accidents_modele$long < -5.5 | accidents_modele$long > 10))
)

for (nom in names(test_coherence)) {
  anom <- test_coherence[[nom]]
  cat(sprintf("   - %-36s : %s\n", nom, ifelse(anom == 0, "CONFORME", sprintf("ATTENTION (%d)", anom))))
}
cat("=======================================================\n\n")

# Export de la table consolidée pour la modélisation sous R et Python
write.csv(accidents_modele, "Ressources/accidents_modele.csv", row.names = FALSE)

cat(sprintf("--> SUCCÈS : Table propre exportée dans Ressources/accidents_modele.csv !\n\n"))
