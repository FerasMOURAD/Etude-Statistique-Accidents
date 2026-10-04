# ==============================================================================
# PIPELINE DE PRÉPARATION ET D'ENRICHISSEMENT DES DONNÉES BAAC 2024
# ==============================================================================

# Se positionner sur la racine du projet si exécuté depuis Code_R
if (basename(getwd()) == "Code_R") {
  setwd("..")
}

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

cat("--> [1/6] Chargement des fichiers bruts...\n")

caracteristiques_2024 <- read.csv("Ressources/caract-2024.csv", sep = ";", header = TRUE)
lieux_2024            <- read.csv("Ressources/lieux-2024.csv", sep = ";", header = TRUE)
vehicules_2024        <- read.csv("Ressources/vehicules-2024.csv", sep = ";", header = TRUE)
usagers_2024          <- read.csv("Ressources/usagers-2024.csv", sep = ";", header = TRUE)

# Chargement et nettoyage de la population INSEE par département
population <- read.csv("Ressources/Insee.csv") %>%
  select(
    dep = Code.département,
    nom_dep = Nom.du.département,
    population = Population.municipale
  ) %>%
  mutate(
    dep = trimws(as.character(dep)),
    population = as.numeric(gsub("[, ]", "", population))
  )

# ------------------------------------------------------------------------------
# 2. Variable Cible : Nombre de victimes par accident (usagers non indemnes)
# ------------------------------------------------------------------------------
cat("--> [2/6] Calcul de la variable cible (Nb_victimes)...\n")

nb_victimes_par_accident <- usagers_2024 %>%
  group_by(Num_Acc) %>%
  filter(grav != 1) %>% # grav 1 = indemne
  summarise(Nb_victimes = n(), .groups = "drop")

accidents_2024 <- caracteristiques_2024 %>%
  left_join(nb_victimes_par_accident, by = "Num_Acc") %>%
  mutate(Nb_victimes = ifelse(is.na(Nb_victimes), 0, Nb_victimes))

accidents_2024$adr <- NULL

# Suppression des 6 accidents avec collision non renseignée (-1)
accidents_2024 <- accidents_2024 %>%
  filter(col != -1)

# ------------------------------------------------------------------------------
# 3. Variables temporelles et Labels des catégories
# ------------------------------------------------------------------------------
cat("--> [3/6] Variables temporelles et labels...\n")

# Saisonnalité ordonnée chronologiquement
accidents_2024 <- accidents_2024 %>%
  mutate(
    saison = case_when(
      mois %in% c(12, 1, 2)  ~ "Hiver",
      mois %in% c(3, 4, 5)   ~ "Printemps",
      mois %in% c(6, 7, 8)   ~ "Ete",
      mois %in% c(9, 10, 11) ~ "Automne"
    ),
    saison = factor(saison, levels = c("Hiver", "Printemps", "Ete", "Automne")),
    atm = as.factor(atm)
  )


# Encodage du departement
accidents_2024$dep_code <- as.integer(as.factor(accidents_2024$dep))


# Date et jour de la semaine
accidents_2024$date <- as.Date(
  paste(accidents_2024$an, accidents_2024$mois, accidents_2024$jour, sep = "-")
)

accidents_2024$jour_semaine <- factor(
  weekdays(accidents_2024$date),
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
  labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
)

# Extraction de l'heure numérique et suppression de hrmn (texte brut)
accidents_2024$heure <- as.numeric(substr(accidents_2024$hrmn, 1, 2))
accidents_2024$hrmn  <- NULL

# Jointure population (appariement 100% propre avec les codes 01-09 préservés)
accidents_2024 <- accidents_2024 %>% left_join(population, by = "dep")

# Dictionnaires de labels pour les graphiques
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

# ------------------------------------------------------------------------------
# 4. Imputation de la Vitesse Maximale Autorisée (vma)
# ------------------------------------------------------------------------------
cat("--> [4/6] Imputation de la VMA (règle métier & dédoublonnage des carrefours)...\n")

vma_acc <- lieux_2024 %>%
  select(Num_Acc, vma) %>%
  left_join(caracteristiques_2024 %>% select(Num_Acc, agg), by = "Num_Acc") %>%
  mutate(
    # Étape 1 : correction des erreurs de frappe (500 -> 50, 900 -> 90)
    vma = ifelse(vma %in% c(300, 500, 700, 800, 900), vma / 10, vma),
    # Étape 2 : détection des valeurs aberrantes ou non renseignées
    vma = ifelse(vma <= 0 | vma > 130, NA, vma),
    # Étape 3 : imputation métier selon la zone (Code de la route)
    vma = case_when(
      !is.na(vma) ~ vma,
      agg == 2    ~ 50, # 50 km/h en ville
      agg == 1    ~ 80, # 80 km/h hors agglomération
      TRUE        ~ 50
    )
  ) %>%
  # Dédoublonnage des intersections : retenir la vitesse max engagée
  group_by(Num_Acc) %>%
  summarise(vma = max(vma), .groups = "drop")

# ------------------------------------------------------------------------------
# 5. Enrichissement Véhicules (Cas A) et Usagers
# ------------------------------------------------------------------------------
cat("--> [5/6] Enrichissement véhicules et usagers...\n")

vehic_acc <- vehicules_2024 %>%
  group_by(Num_Acc) %>%
  summarise(
    nb_vehicules_impliquees = n(),
    implique_velo_edp      = as.integer(any(catv %in% c(1, 50, 60, 80))),
    implique_voiture       = as.integer(any(catv %in% c(3, 7, 8, 9, 10, 11, 12))),
    implique_moto          = as.integer(any(catv %in% c(2, 4, 5, 6, 30, 31, 32, 33, 34, 35, 36, 41, 42, 43))),
    implique_poids_lourd   = as.integer(any(catv %in% c(13, 14, 15, 16, 17, 20, 21))),
    implique_transp_commun = as.integer(any(catv %in% c(18, 19, 37, 38, 39, 40))),
    .groups = "drop"
  )

usagers_acc <- usagers_2024 %>%
  group_by(Num_Acc) %>%
  summarise(
    implique_pieton = as.integer(any(catu == 3)),
    defaut_securite = as.integer(any(secu1 == 0)), # 1 si au moins un usager sans ceinture/casque
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 6. Jointure finale & Export
# ------------------------------------------------------------------------------
cat("--> [6/6] Finalisation de la table accidents_2024...\n")

accidents_2024 <- accidents_2024 %>%
  left_join(vma_acc, by = "Num_Acc") %>%
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
    defaut_securite         = coalesce(defaut_securite, 0L)
  )

# Export pour le notebook Python (Data Mining)
write.csv(accidents_2024, "Ressources/accidents_2024.csv", row.names = FALSE)

cat(sprintf("--> SUCCÈS : %d accidents préparés et exportés dans Ressources/accidents_2024.csv !\n", nrow(accidents_2024)))
