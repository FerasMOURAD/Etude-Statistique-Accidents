library(dplyr)
library(ggplot2)

setwd("/home/fm/Desktop/")

caracteristiques_2024 = read.csv("caract-2024.csv", sep = ";", header = TRUE)
lieux_2024 = read.csv("lieux-2024.csv", sep = ";", header = TRUE)
vehicules_2024 = read.csv("vehicules-2024.csv", sep = ";", header = TRUE)
usagers_2024 = read.csv("usagers-2024.csv", sep = ";", header = TRUE)


nb_victimes_par_accident = usagers_2024 %>%
  group_by(Num_Acc) %>%
  filter(grav != 1) %>%
  summarise(Nombre_Victimes = n())
print(nb_victimes_par_accident)


accidents_2024 <- caracteristiques_2024 %>%
  left_join(nb_victimes_par_accident, by = "Num_Acc")

head(accidents_2024)

#on check les valeurs manquantes (s'il y en a)
valeurs_manquantes <- colSums(is.na(accidents_2024))
valeurs_manquantes

