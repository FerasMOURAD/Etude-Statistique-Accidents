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




#on extrait la date de l'accident
accidents_2024$date = as.Date(
  paste(accidents_2024$an, accidents_2024$mois, accidents_2024$jour, 
  sep = "-"))

accidents_2024$jour_semaine = factor(
  weekdays(accidents_2024$date),
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
  labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
)


# ANALYSE UNIVARIÉE DE TOUTES LES VARIABLES DE LA TABLE ACCIDENTS_2024

# 1. VARIABLES TEMPORELLES
# Répartition mensuelle
table(accidents_2024$mois)
round(prop.table(table(accidents_2024$mois)) * 100, 2)

# Répartition par jour de la semaine
table(accidents_2024$jour_semaine)
round(prop.table(table(accidents_2024$jour_semaine)) * 100, 2)

# 2. VARIABLES ENVIRONNEMENTALES
# Luminosité (1 = Plein jour, 2 = Crépuscule/aube, 3 = Nuit sans éclairage, 5 = Nuit éclairée)
table(accidents_2024$lum)
round(prop.table(table(accidents_2024$lum)) * 100, 2)

table(accidents_2024$atm)
round(prop.table(table(accidents_2024$atm)) * 100, 2)

table(accidents_2024$saison)
round(prop.table(table(accidents_2024$saison)) * 100, 2)

# 3. VARIABLES D'INFRASTRUCTURE ET DE COLLISION
# Agglomération (1 = Hors agglomération / campagne-autoroute, 2 = En agglomération / ville)
table(accidents_2024$agg)
round(prop.table(table(accidents_2024$agg)) * 100, 2)

# Type d'intersection (1 = Hors intersection, 2 = En X, 3 = En T, 6 = Rond-point)
table(accidents_2024$int)
round(prop.table(table(accidents_2024$int)) * 100, 2)

# Type de collision (1 = Frontal, 2 = Arrière, 3 = Côté, 4 = En chaîne, 7 = Sans collision)
table(accidents_2024$col)
round(prop.table(table(accidents_2024$col)) * 100, 2)

# 4. VARIABLE GÉOGRAPHIQUE
# Top 10 des départements les plus accidentogènes
top_10_dep = sort(table(accidents_2024$dep), decreasing = TRUE)[1:10]
top_10_dep
round(prop.table(top_10_dep) * 100, 2)

