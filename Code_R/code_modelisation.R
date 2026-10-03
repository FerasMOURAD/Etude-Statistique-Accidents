# ==============================================================================
# SCRIPT PRINCIPAL : ANALYSES STATISTIQUES ET MODÉLISATION
# ==============================================================================

# Se positionner sur la racine du projet si exécuté depuis Code_R
if (basename(getwd()) == "Code_R") {
  setwd("..")
}

# 1. Chargement automatique des données nettoyées et enrichies
source("Code_R/preparation_donnees.R")

library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 2. ANALYSES ET GRAPHES
# ------------------------------------------------------------------------------

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
       x = "Nombre d'accidents")





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
ggplot(accidents_2024 %>% filter(!is.na(jour_semaine)), aes(x = jour_semaine)) +
  geom_bar(fill = "#5C6BC0") +
  labs(title = "Nombre d'accidents par jour de la semaine",
       x = "Jour de la semaine", 
       y = "Nombre d'accidents") +
  theme_minimal()

#heure
ggplot(accidents_2024 %>% filter(!is.na(heure)), 
       aes(x = heure)) +
  geom_bar(fill = "#FF7043", color = "white") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(title = "Distribution des accidents par heure de la journée",
       x = "Heure de la journée", 
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
  labs(title = "Types de collision",
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



