library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/home/fm/Desktop/Me/Uni_Stuff/BUTSD/Etude-Statistique-Accidents/")

caracteristiques_2024 = read.csv("Ressources/caract-2024.csv", sep = ";", header = TRUE)
lieux_2024 = read.csv("Ressources/lieux-2024.csv", sep = ";", header = TRUE)
vehicules_2024 = read.csv("Ressources/vehicules-2024.csv", sep = ";", header = TRUE)
usagers_2024 = read.csv("Ressources/usagers-2024.csv", sep = ";", header = TRUE)

# Chargement de la population INSEE par département
population = read.csv("Ressources/Insee.csv") %>%
  select(
    dep = Code.département,
    nom_dep = Nom.du.département,
    population = Population.municipale
  ) %>%
  mutate(
    dep = sub("^0+", "", as.character(dep)),
    population = as.numeric(gsub(",", "", population))
  )

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
colSums(accidents_2024 == -1, na.rm = T)

# Suppression des 6 accidents avec collision non renseignée (-1)
accidents_2024 <- accidents_2024 %>%
  filter(col != -1)



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
round(sd(accidents_2024$Nb_victimes), 2)


accidents_2024 = accidents_2024 %>% left_join(population, by = "dep")

#on met des labels pour les categories.
# Conditions atmosphériques (atm)
lab_atm <- c("1" = "Normale", "2" = "Pluie légère", "3" = "Pluie forte",
             "4" = "Neige/Grêle", "5" = "Brouillard", "6" = "Vent fort",
             "7" = "Temps éblouissant", "8" = "Temps nuageux", "9" = "Autre")

# Conditions de luminosité (lum)
lab_lum <- c("1" = "Plein jour", "2" = "Crépuscule/aube",
             "3" = "Nuit sans éclairage", "4" = "Nuit avec éclairage",
             "5" = "Nuit éclairage allumé")

# Type de collision (col)
lab_col = c("1" = "Deux véh. - frontale",
             "2" = "Deux véh. - par l'arrière",
             "3" = "Deux véh. - par le côté",
             "4" = "Trois véh.+ en chaîne",
             "5" = "Trois véh.+ conf. multiple",
             "6" = "Autre collision",
             "7" = "Sans collision")

lab_int = c("1" = "Hors intersection", "2" = "En X", "3" = "En T",
             "4" = "En Y", "5" = "> 4 branches", "6" = "Rond-point",
             "7" = "Place", "8" = "Passage à niveau", "9" = "Autre")


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

# on représente le nombre de victimes par accident
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

# VARIABLES TEMPORELLES

# Répartition mensuelle
table(accidents_2024$mois)
round(prop.table(table(accidents_2024$mois)) * 100, 2)
ggplot(accidents_2024 %>% filter(!is.na(mois)),
       aes(x = factor(mois,
                      levels = 1:12,
                      labels = c("Jan","Fév","Mar","Avr","Mai","Jun",
                                 "Jul","Aoû","Sep","Oct","Nov","Déc")))) +
  geom_bar(fill = "#66BB6A") +
  labs(title = "Nombre d'accidents par mois",
       x = "Mois", y = "Nombre d'accidents") +
  theme_minimal()


# Répartition par jour de la semaine
table(accidents_2024$jour_semaine)
round(prop.table(table(accidents_2024$jour_semaine)) * 100, 2)


#heure
ggplot(accidents_2024 %>% filter(!is.na(hrmn)), 
       aes(x = as.numeric(substr(hrmn, 1, 2)))) +
  geom_bar(fill = "#FF7043", color = "white") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(title = "Distribution des accidents par heure de la journée",
       x = "Heure de la journée (0h à 23h)", 
       y = "Nombre d'accidents") +
  theme_minimal()


# 2. VARIABLES ENVIRONNEMENTALES
# Luminosité (1 = Plein jour, 2 = Crépuscule/aube, 3 = Nuit sans éclairage, 5 = Nuit éclairée)
table(accidents_2024$lum)
round(prop.table(table(accidents_2024$lum)) * 100, 2)
ggplot(accidents_2024 %>% filter(!is.na(lum)), aes(x = factor(lum))) +
  geom_bar(fill = "#FFCA28") +
  scale_x_discrete(labels = lab_lum) +
  labs(title = "Conditions de luminosité lors des accidents",
       x = "", y = "Nombre d'accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



#ATM
table(accidents_2024$atm)
round(prop.table(table(accidents_2024$atm)) * 100, 2)
ggplot(accidents_2024 %>% filter(!is.na(atm)), aes(x = factor(atm))) +
  geom_bar(fill = "#42A5F5") +
  scale_x_discrete(labels = lab_atm) +
  labs(title = "Conditions atmosphériques lors des accidents",
       x = "", y = "Nombre d'accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#Saison
table(accidents_2024$saison)
round(prop.table(table(accidents_2024$saison)) * 100, 2)
ggplot(accidents_2024, aes(x = saison, fill = saison)) +
  geom_bar(aes(fill = factor(saison))) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.5) +
  labs(title = "Nombre d'accidents par saison",
       x = "Saison", y = "Nombre d'accidents") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(legend.position="none")

# 3. VARIABLES D'INFRASTRUCTURE ET DE COLLISION

# Agglomération (1 = Hors agglomération / campagne-autoroute, 2 = En agglomération / ville)
table(accidents_2024$agg)
round(prop.table(table(accidents_2024$agg)) * 100, 2)
ggplot(accidents_2024 %>% filter(!is.na(agg)),
       aes(x = factor(agg,
                      levels = 1:2,
                      labels = c("Hors agglomération","En agglomération")))) +
  geom_bar(fill = "#66BB6A") +
  labs(title = "Nombre d'accidents par agg",
       x = "Agglomération", y = "Nombre d'accidents") +
  theme_minimal()


# Type d'intersection (1 = Hors intersection, 2 = En X, 3 = En T, 6 = Rond-point)
table(accidents_2024$int)
round(prop.table(table(accidents_2024$int)) * 100, 2)
ggplot(accidents_2024 %>% filter(!is.na(int)), aes(x = factor(int))) +
  geom_bar(fill = "#26A69A") +
  scale_x_discrete(labels = lab_int) +
  labs(title = "Types d'intersections lors des accidents",
       x = "", y = "Nombre d'accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Type de collision (1 = Frontal, 2 = Arrière, 3 = Côté, 4 = En chaîne, 7 = Sans collision)
table_col <- table(case_when(
  accidents_2024$col %in% 1:3 ~ "Deux véhicules",
  accidents_2024$col %in% 4:5 ~ "Trois véhicules et plus",
  accidents_2024$col == 6     ~ "Autre collision",
  accidents_2024$col == 7     ~ "Sans collision"
))
table_col
round(prop.table(table_col) * 100, 2)

ggplot(accidents_2024 %>% 
         filter(!is.na(col)) %>%
         mutate(col_regroup = case_when(
           col %in% 1:3 ~ "Deux véhicules",
           col %in% 4:5 ~ "Trois véhicules et plus",
           col == 6     ~ "Autre collision",
           col == 7     ~ "Sans collision"
         )), 
       aes(x = factor(col_regroup, 
                      levels = c("Deux véhicules", 
                                 "Trois véhicules et plus", 
                                 "Autre collision", 
                                 "Sans collision")))) +
  geom_bar(fill = "#AB47BC") +
  labs(title = "Types de collision (regroupés)",
       x = "", y = "Nombre d'accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

 

# 4. VARIABLE GÉOGRAPHIQUE
# Top 20 départements les plus accidentogènes pour 100 000 habitants
top_dep <- accidents_2024 %>%
  filter(!is.na(dep)) %>%
  count(dep, nom_dep, population, name = "nb_accidents") %>%
  mutate(ratio = round((nb_accidents / population) * 100000, 2)) %>%
  arrange(desc(ratio)) %>%
  slice_head(n = 20)
print(top_dep)

ggplot(top_dep, aes(x = reorder(nom_dep, ratio), y = ratio)) +
  geom_col(fill = "#AB47BC") +
  coord_flip() +
  labs(title = "Top 20 départements les plus accidentogènes",
       x = "Département", y = "Nombre d'accidents pour 100 000 habitants") +
  theme_minimal()

