# ==============================================================================
# 1. INSTALLATION ET CHARGEMENT DES PACKAGES
# ==============================================================================
# Décommentez (enlevez le #) les deux lignes suivantes si les packages ne sont pas encore installés
# install.packages("dplyr")
# install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# ==============================================================================
# 2. DÉFINITION DU RÉPERTOIRE DE TRAVAIL
# ==============================================================================
# Remplacez le chemin ci-dessous par le dossier où sont rangés vos fichiers
# setwd("C:/Chemin/Vers/Vos/Fichiers/2024")

# ==============================================================================
# 3. IMPORTATION DES DONNÉES 2024
# ==============================================================================
# Le séparateur standard des données françaises est le point-virgule (sep = ";")
caracteristiques_2024 <- read.csv("caract-2024_excel.csv", sep = ";", header = TRUE)
lieux_2024 <- read.csv("lieux-2024_excel.csv", sep = ";", header = TRUE)
vehicules_2024 <- read.csv("vehicules-2024_excel.csv", sep = ";", header = TRUE)
usagers_2024 <- read.csv("usagers-2024_excel.csv", sep = ";", header = TRUE)

# ==============================================================================
# 4. VÉRIFICATION DE L'IMPORTATION
# ==============================================================================
# La fonction glimpse() du package dplyr offre un aperçu très lisible de vos tables
View(caracteristiques_2024)
View(usagers_2024)


# 1. Calcul du nombre total de victimes par accident (à partir de la table usagers)
nb_victimes_par_accident <- usagers_2024 %>%
  group_by(Num_Acc) %>%
  summarise(Nombre_Victimes = n())
print(nb_victimes_par_accident)

# 2. Fusion avec la table des caractéristiques géographiques et environnementales
accidents_2024 <- caracteristiques_2024 %>%
  left_join(nb_victimes_par_accident, by = "Num_Acc")

# Un petit aperçu de votre nouvelle table consolidée
glimpse(accidents_2024)




# 1. Compter le nombre de valeurs manquantes pour chaque colonne de la table
valeurs_manquantes <- colSums(is.na(accidents_2024))

# Afficher les colonnes qui ont au moins une valeur manquante
valeurs_manquantes[valeurs_manquantes > 0]

# 2. Focus sur les données spatiales (si les colonnes s'appellent bien lat et long)
# Pourcentage de coordonnées GPS manquantes
pct_na_lat <- sum(is.na(accidents_2024$lat)) / nrow(accidents_2024) * 100
cat("Pourcentage de latitudes manquantes :", round(pct_na_lat, 2), "%\n")