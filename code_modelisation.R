library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/home/fm/Desktop/Me/Personal/Projects/Uni/Etude-Statistique-Accidents")

caracteristiques_2024 = read.csv("Ressources/caract-2024.csv", sep = ";", header = TRUE)
lieux_2024 = read.csv("Ressources/lieux-2024.csv", sep = ";", header = TRUE)
vehicules_2024 = read.csv("Ressources/vehicules-2024.csv", sep = ";", header = TRUE)
usagers_2024 = read.csv("Ressources/usagers-2024.csv", sep = ";", header = TRUE)

#on calcule le nb de victimes par accident
nb_victimes_par_accident = usagers_2024 %>%
  group_by(Num_Acc) %>%
  filter(grav != 1) %>%
  summarise(Nb_victimes = n())
print(nb_victimes_par_accident)

#accidents 2024 c'est la meme que caracteristiques_2024, mais on a ajoute
#le nombre de victimes par accident a cette dernière
accidents_2024 = caracteristiques_2024 %>%
  left_join(nb_victimes_par_accident, by = "Num_Acc") %>%
  mutate (Nb_victimes = replace_na(Nb_victimes, 0))

head(accidents_2024)

#on check les valeurs manquantes (s'il y en a)
# valeurs_manquantes <- colSums(is.na(accidents_2024))
# valeurs_manquantes

#on cree la saisonnalite
accidents_2024 = accidents_2024 %>%
  mutate(saison = case_when(
    mois %in% c(12,1,2) ~ "Hiver",
    mois %in% c(3,4,5) ~ "Printemps",
    mois %in% c(6,7,8) ~ "Ete",
    mois %in% c(9,10,11) ~ "Automne"
  ),
  #pour ne pas considerer les mois comme une variable quantitative
  saison = as.factor(saison)
)

head(accidents_2024)
