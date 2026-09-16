library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/home/fm/Desktop/Me/Uni_Stuff/BUTSD/Etude-Statistique-Accidents/")

caracteristiques_2024 = read.csv("Ressources/caract-2024.csv", sep = ";", header = TRUE)
lieux_2024 = read.csv("Ressources/lieux-2024.csv", sep = ";", header = TRUE)
vehicules_2024 = read.csv("Ressources/vehicules-2024.csv", sep = ";", header = TRUE)
usagers_2024 = read.csv("Ressources/usagers-2024.csv", sep = ";", header = TRUE)

#on calcule le nb de victimes par accident
nb_victimes_par_accident = usagers_2024 %>%
  group_by(Num_Acc) %>%
  filter(grav != 1) %>%
  summarise(Nb_victimes = n())
nb_victimes_par_accident

#accidents_2024 c'est la même que caracteristiques_2024, mais on a ajouté
#le nombre de victimes par accident a cette dernière
accidents_2024 = caracteristiques_2024 %>%
  left_join(nb_victimes_par_accident, by = "Num_Acc")

accidents_2024$adr = NULL

#on verifie s'il y a des champs vides, des champs NA, des champs N/A
colSums(accidents_2024 == "")
colSums(accidents_2024 == "N/A")
colSums(is.na(accidents_2024))

head(accidents_2024)


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
accidents_2024$atm = as.factor(accidents_2024$atm)
head(accidents_2024)

summary(accidents_2024$Nb_victimes)
sd(accidents_2024$Nb_victimes)

# l'analyse univariée : on représente le nombre de victimes par accident
accidents_2024 %>%
  count(Nb_victimes) %>%
  ggplot() +
  geom_rect(aes(ymin = Nb_victimes - 0.4, ymax = Nb_victimes + 0.4, xmin = 0.5, xmax = n),
            fill = "steelblue", color = "white") +
  scale_x_log10(limits = c(0.5, 50000), breaks = c(1, 10, 100, 1000, 10000),
                labels = c("1", "10", "100", "1 000", "10 000")) +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20, 30, 40, 50)) +
  labs(title = "Distribution du nombre de victimes par accident",
       y = "Nombre de victimes",
       x = "Nombre d'accidents (échelle log10)")



