# ==============================================================================
# PROJET D'ÉTUDE STATISTIQUE SUR LES ACCIDENTS CORPORELS (FRANCE - 2024)
# SCRIPT PRINCIPAL : ANALYSES EXPLORATOIRES, INFÉRENCE ET ANALYSE SPATIALE
# ==============================================================================
#
# SOMMAIRE DU SCRIPT :
# ------------------------------------------------------------------------------
# SECTION 1 : ENVIRONNEMENT, LIBRAIRIES ET CHARGEMENT DES DONNÉES
#
# SECTION 2 : ANALYSES UNIVARIÉES (DISTRIBUTIONS DES VARIABLES)
#   2.1 Variable Cible : Nombre de victimes par accident (Nb_victimes)
#   2.2 Variables Temporelles : Mois, Jour de semaine, Heure et Saison
#   2.3 Variables Environnementales : Luminosité et Conditions atmosphériques
#   2.4 Variables d'Infrastructure et de Choc : Agglomération, Intersections et Collisions
#   2.5 Variable Géographique : Taux d'accidents pour 100 000 habitants (Top 20)
#
# SECTION 3 : ANALYSES BIVARIÉES ET INFÉRENCE STATISTIQUE
#   3.1 Facteurs explicatifs vs Nombre de victimes (X x Y)
#       3.1.1 Saison vs Nb_victimes (Quali x Quanti)
#       3.1.2 Conditions atmosphériques vs Nb_victimes (Quali x Quanti)
#       3.1.3 Agglomération (Zone) vs Nb_victimes (Quali x Quanti)
#       3.1.4 Luminosité vs Nb_victimes (Quali x Quanti)
#       3.1.5 Type de collision vs Nb_victimes (Quali x Quanti)
#       3.1.6 Vitesse Maximale Autorisée (VMA) vs Nb_victimes
#       3.1.7 Profil des usagers vs Nb_victimes (Jeunes, Seniors, Hommes)
#       3.1.8 Obstacles et état de la chaussée vs Nb_victimes
#   3.2 Liaisons entre variables explicatives
#       3.2.1 à 3.2.6 Croisements qualitatifs (Chi-2 et V de Cramer)
#       3.2.7 Vitesse (VMA) selon la zone (Quanti x Quali)
#   3.3 Croisement Quantitative x Quantitative (Pearson et Spearman)
#       3.3.1 VMA continue vs Nb_victimes
#       3.3.2 Âge moyen des usagers vs Nb_victimes
#       3.3.3 Nombre de véhicules impliqués vs Nb_victimes
#
# SECTION 4 : MODÉLISATION LINÉAIRE MULTIPLE (apprentissage 70 % / test 30 %)
#   4.1 Estimation du modèle linéaire complet
#   4.2 Sélection pas-à-pas de variables (stepAIC)
#   4.3 Diagnostic graphique des résidus
#   4.4 Conditions de validité (KS, Breusch-Pagan, Cook, leviers)
#   4.5 Recherche du lambda de Box-Cox
#   4.6 Transformations racine et log : RMSE et MAE sur le jeu de test
#   4.7 Erreurs standard robustes HC3
#   4.8 Sensibilité à la variable nb_usagers
#   4.9 Observations influentes (Cook > 1)
#   4.10 Test direct de la saison et de la météo (drop1)
#
# SECTION 5 : ANALYSE SPATIALE (Moran, Geary, cartes)
#   5.1 Tableau départemental : victimes, usagers, résidus du modèle
#   5.2 Voisinage par k plus proches voisins
#   5.3 Moran et Geary (k = 5) et sensibilité à k
#   5.4 Dépendance spatiale des résidus et du nombre d'usagers
#   5.5 Contours des départements et voisinage par frontières communes
#   5.6 Moran et Geary avec les frontières communes
#   5.7 Cartes en aplats de couleurs
# ==============================================================================

# Se positionner sur la racine du projet si exécuté depuis Code_R
if (basename(getwd()) == "Code_R") {
  setwd("..")
}

# ==============================================================================
# SECTION 1 : ENVIRONNEMENT, LIBRAIRIES ET CHARGEMENT DES DONNÉES
# ==============================================================================

cat("\n=======================================================\n")
cat("  SECTION 1 : ENVIRONNEMENT ET CHARGEMENT DES DONNÉES  \n")
cat("=======================================================\n")

# 1. Chargement et exécution du pipeline ETL
source("Code_R/preparation_donnees.R")

# 2. Librairies statistiques et graphiques requises
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

library(MASS)
library(sandwich)
library(spdep)
library(lmtest)
library(sf)
library(ggspatial)

cat("--> Environnement prêt :", nrow(accidents_modele), "observations chargées.\n")


# ==============================================================================
# SECTION 2 : ANALYSES UNIVARIÉES (DISTRIBUTIONS DES VARIABLES)
# ==============================================================================

cat("\n=======================================================\n")
cat("          SECTION 2 : ANALYSES UNIVARIÉES             \n")
cat("=======================================================\n")

# ------------------------------------------------------------------------------
# 2.1 Variable Cible : Nombre de victimes par accident (usagers non indemnes)
# ------------------------------------------------------------------------------
cat("\n--- 2.1 Distribution de la variable cible (Nb_victimes) ---\n")
print(table(accidents_modele$Nb_victimes))
cat("\nFréquences relatives (% d'accidents) :\n")
print(round(prop.table(table(accidents_modele$Nb_victimes)) * 100, 2))

accidents_modele %>%
  count(Nb_victimes) %>%
  ggplot() +
  geom_rect(aes(ymin = Nb_victimes - 0.4, ymax = Nb_victimes + 0.4, xmin = 0.5, xmax = n),
            fill = "steelblue", color = "white") +
  scale_x_log10(limits = c(0.5, 50000), breaks = c(1, 10, 100, 1000, 10000),
                labels = c("1", "10", "100", "1 000", "10 000")) +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20, 30, 40, 50)) +
  labs(
    title = "Distribution du nombre de victimes par accident",
    subtitle = "Échelle logarithmique sur l'axe des effectifs",
    x = "Nombre d'accidents (log10)",
    y = "Nombre de victimes"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 2.2 Variables Temporelles : Mois, Jour de la semaine, Heure et Saison
# ------------------------------------------------------------------------------
cat("\n--- 2.2 Répartition temporelle ---\n")

# Répartition mensuelle
cat("\n* Répartition par mois :\n")
print(round(prop.table(table(accidents_modele$mois)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(mois)),
       aes(x = factor(mois, levels = 1:12,
                      labels = c("Jan","Fév","Mar","Avr","Mai","Jun",
                                 "Jul","Aoû","Sep","Oct","Nov","Déc")))) +
  geom_bar(fill = "#66BB6A") +
  labs(title = "Nombre d'accidents par mois (2024)", x = "Mois", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12)

# Répartition par jour de la semaine
cat("\n* Répartition par jour de la semaine :\n")
print(round(prop.table(table(accidents_modele$jour_semaine)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(jour_semaine)), aes(x = jour_semaine)) +
  geom_bar(fill = "#5C6BC0") +
  labs(title = "Nombre d'accidents par jour de la semaine",
       x = "Jour de la semaine", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12)

# Répartition par heure de la journée
ggplot(accidents_modele %>% filter(!is.na(heure)), aes(x = heure)) +
  geom_bar(fill = "#FF7043", color = "white") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(title = "Distribution des accidents par heure de la journée",
       subtitle = "Pics nets observés aux heures de pointe pendulaires",
       x = "Heure de la journée", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12)

# Répartition par saison
cat("\n* Répartition par saison :\n")
print(round(prop.table(table(accidents_modele$saison)) * 100, 2))

ggplot(accidents_modele, aes(x = saison, fill = saison)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.5) +
  labs(title = "Nombre d'accidents par saison", x = "Saison", y = "Nombre d'accidents") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# 2.3 Variables Environnementales : Luminosité et Météo
# ------------------------------------------------------------------------------
cat("\n--- 2.3 Conditions environnementales ---\n")

# Luminosité
cat("\n* Conditions de luminosité :\n")
print(round(prop.table(table(accidents_modele$lum_label)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(lum_label)), aes(x = lum_label)) +
  geom_bar(fill = "#FFCA28") +
  labs(title = "Conditions de luminosité lors des accidents", x = "", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Conditions atmosphériques (Météo)
cat("\n* Conditions atmosphériques :\n")
print(round(prop.table(table(accidents_modele$atm_label)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(atm_label)), aes(x = atm_label)) +
  geom_bar(fill = "#42A5F5") +
  labs(title = "Conditions atmosphériques lors des accidents", x = "", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ------------------------------------------------------------------------------
# 2.4 Variables d'Infrastructure et de Collision
# ------------------------------------------------------------------------------
cat("\n--- 2.4 Infrastructure et type de collision ---\n")

# Agglomération (Zone)
cat("\n* Répartition selon la zone (agglomération vs rurale) :\n")
print(round(prop.table(table(accidents_modele$agg_label)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(agg_label)), aes(x = agg_label)) +
  geom_bar(fill = "#66BB6A") +
  labs(title = "Nombre d'accidents selon la zone", x = "Agglomération", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12)

# Type d'intersection
cat("\n* Types d'intersections :\n")
print(round(prop.table(table(accidents_modele$int)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(int)), aes(x = factor(int))) +
  geom_bar(fill = "#26A69A") +
  scale_x_discrete(labels = lab_int) +
  labs(title = "Types d'intersections lors des accidents", x = "", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Typologie de collision (6 catégories : regroupement des chocs 2 et 3 sans composante frontale)
cat("\n* Typologie de collision :\n")
print(round(prop.table(table(accidents_modele$col_regroup)) * 100, 2))

ggplot(accidents_modele %>% filter(!is.na(col_regroup)), aes(x = col_regroup)) +
  geom_bar(fill = "#AB47BC") +
  labs(title = "Distribution des accidents selon le type de collision",
       subtitle = "Nomenclature consolidée en 6 catégories homogènes",
       x = "", y = "Nombre d'accidents") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# ------------------------------------------------------------------------------
# 2.5 Variable Géographique : Top 20 départements pour 100 000 habitants
# ------------------------------------------------------------------------------
cat("\n--- 2.5 Taux d'accidents départemental pour 100 000 habitants ---\n")

top_dep <- accidents_modele %>%
  filter(!is.na(population) & !grepl("^9[78]", dep)) %>%
  count(dep, nom_dep, population, name = "nb_accidents") %>%
  mutate(ratio = round((nb_accidents / population) * 100000, 2)) %>%
  arrange(desc(ratio)) %>%
  slice_head(n = 20)

print(top_dep)

ggplot(top_dep, aes(x = reorder(nom_dep, ratio), y = ratio)) +
  geom_col(fill = "#AB47BC") +
  coord_flip() +
  labs(title = "Top 20 des départements les plus accidentogènes",
       subtitle = "Taux standardisé pour 100 000 habitants (INSEE 2024)",
       x = "Département", y = "Nombre d'accidents pour 100 000 habitants") +
  theme_minimal(base_size = 12)


# ==============================================================================
# SECTION 3 : ANALYSES BIVARIÉES ET INFÉRENCE STATISTIQUE
# ==============================================================================

cat("\n=======================================================\n")
cat("      SECTION 3 : ANALYSES BIVARIÉES ET INFÉRENCE      \n")
cat("=======================================================\n")

# ==============================================================================
# 3.1 Facteurs explicatifs vs Nombre de victimes (X x Y)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1.1 Saison vs Nb_victimes (Quali x Quanti)
# ------------------------------------------------------------------------------
cat("\n=== 3.1.1 Saison x Nb_victimes ===\n")

stats_saison <- accidents_modele %>%
  filter(!is.na(saison)) %>%
  group_by(saison) %>%
  summarise(
    Effectif   = n(),
    Moyenne    = round(mean(Nb_victimes, na.rm = TRUE), 3),
    Ecart_type = round(sd(Nb_victimes, na.rm = TRUE), 3),
    Mediane    = median(Nb_victimes, na.rm = TRUE),
    Q1         = quantile(Nb_victimes, probs = 0.25, na.rm = TRUE),
    Q3         = quantile(Nb_victimes, probs = 0.75, na.rm = TRUE)
  )
print("Statistiques descriptives par saison :")
print(stats_saison)

# Graphique du nombre moyen de victimes par saison
ggplot(stats_saison, aes(x = saison, y = Moyenne, fill = saison)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1.6)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Nombre moyen de victimes par accident selon la saison",
    x = "Saison", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# Tests d'hypothèses
cat("\n* Test de normalité de Kolmogorov-Smirnov par saison :\n")
ks_saison <- lapply(levels(accidents_modele$saison), function(s) {
  vals <- accidents_modele$Nb_victimes[accidents_modele$saison == s]
  res <- ks.test(vals, pnorm, mean(vals), sd(vals))
  data.frame(Saison = s, D = round(res$statistic, 4), P_valeur = format.pval(res$p.value, digits = 3))
})
print(bind_rows(ks_saison))

cat("\n* Test d'homoscédasticité de Bartlett :\n")
print(bartlett.test(Nb_victimes ~ saison, data = accidents_modele))

cat("\n* Test non-paramétrique de Kruskal-Wallis :\n")
test_kruskal_saison <- kruskal.test(Nb_victimes ~ saison, data = accidents_modele)
print(test_kruskal_saison)

cat("\n* Test ANOVA / Welch généralisé (robuste à l'hétéroscédasticité) :\n")
print(oneway.test(Nb_victimes ~ saison, data = accidents_modele))

# ------------------------------------------------------------------------------
# 3.1.2 Conditions atmosphériques (Météo) vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.1.2 Conditions atmosphériques x Nb_victimes ===\n")

stats_atm <- accidents_modele %>%
  filter(!is.na(atm_label)) %>%
  group_by(atm_label) %>%
  summarise(
    Effectif   = n(),
    Moyenne    = round(mean(Nb_victimes, na.rm = TRUE), 3),
    Ecart_type = round(sd(Nb_victimes, na.rm = TRUE), 3),
    Mediane    = median(Nb_victimes, na.rm = TRUE),
    Q1         = quantile(Nb_victimes, probs = 0.25, na.rm = TRUE),
    Q3         = quantile(Nb_victimes, probs = 0.75, na.rm = TRUE)
  ) %>%
  arrange(desc(Effectif))

print("Statistiques descriptives par condition météorologique :")
print(stats_atm)

# Graphique du nombre moyen de victimes par condition météorologique
ggplot(stats_atm, aes(x = reorder(atm_label, Moyenne), y = Moyenne, fill = atm_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            hjust = -0.1, size = 3.8, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.8)) +
  labs(
    title = "Nombre moyen de victimes selon les conditions atmosphériques",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

cat("\n* Test d'homoscédasticité de Bartlett :\n")
print(bartlett.test(Nb_victimes ~ atm_label, data = accidents_modele %>% filter(!is.na(atm_label))))

cat("\n* Test non-paramétrique de Kruskal-Wallis :\n")
test_kruskal_atm <- kruskal.test(Nb_victimes ~ atm_label, data = accidents_modele %>% filter(!is.na(atm_label)))
print(test_kruskal_atm)

cat("\n* Test de Welch généralisé :\n")
print(oneway.test(Nb_victimes ~ atm_label, data = accidents_modele %>% filter(!is.na(atm_label))))

# ------------------------------------------------------------------------------
# 3.1.3 Agglomération (Zone) vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.1.3 Agglomération x Nb_victimes ===\n")

stats_agg <- accidents_modele %>%
  filter(!is.na(agg_label)) %>%
  group_by(agg_label) %>%
  summarise(
    Effectif   = n(),
    Moyenne    = round(mean(Nb_victimes, na.rm = TRUE), 3),
    Ecart_type = round(sd(Nb_victimes, na.rm = TRUE), 3),
    Mediane    = median(Nb_victimes, na.rm = TRUE),
    Q1         = quantile(Nb_victimes, probs = 0.25, na.rm = TRUE),
    Q3         = quantile(Nb_victimes, probs = 0.75, na.rm = TRUE)
  )
print("Statistiques descriptives : Agglomération vs Nb_victimes :")
print(stats_agg)

# Graphique du nombre moyen de victimes par zone
ggplot(stats_agg, aes(x = agg_label, y = Moyenne, fill = agg_label)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            vjust = -0.5, size = 4.2, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1.8)) +
  scale_fill_manual(values = c("Hors agglomération" = "#EF5350", "En agglomération" = "#42A5F5")) +
  labs(
    title = "Nombre moyen de victimes par accident selon la zone",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

cat("\n* Test de Fisher pour l'égalité des variances :\n")
print(var.test(Nb_victimes ~ agg_label, data = accidents_modele))

cat("\n* Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
test_wilcox_agg <- wilcox.test(Nb_victimes ~ agg_label, data = accidents_modele)
print(test_wilcox_agg)

cat("\n* Test de Welch pour 2 groupes à variances inégales :\n")
print(t.test(Nb_victimes ~ agg_label, data = accidents_modele, var.equal = FALSE))

# ------------------------------------------------------------------------------
# 3.1.4 Luminosité vs Nb_victimes (Quali x Quanti)
# ------------------------------------------------------------------------------
cat("\n=== 3.1.4 Luminosité x Nb_victimes ===\n")

stats_lum <- accidents_modele %>%
  filter(!is.na(lum_label)) %>%
  group_by(lum_label) %>%
  summarise(
    Effectif   = n(),
    Moyenne    = round(mean(Nb_victimes, na.rm = TRUE), 3),
    Ecart_type = round(sd(Nb_victimes, na.rm = TRUE), 3),
    Mediane    = median(Nb_victimes, na.rm = TRUE),
    Q1         = quantile(Nb_victimes, probs = 0.25, na.rm = TRUE),
    Q3         = quantile(Nb_victimes, probs = 0.75, na.rm = TRUE)
  )
print("Statistiques descriptives : Luminosité vs Nb_victimes :")
print(stats_lum)

# Graphique du nombre moyen de victimes selon la luminosité
ggplot(stats_lum, aes(x = reorder(lum_label, Moyenne), y = Moyenne, fill = lum_label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.7)) +
  labs(
    title = "Nombre moyen de victimes selon les conditions de luminosité",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

cat("\n* Test d'homoscédasticité de Bartlett :\n")
print(bartlett.test(Nb_victimes ~ lum_label, data = accidents_modele %>% filter(!is.na(lum_label))))

cat("\n* Test non-paramétrique de Kruskal-Wallis :\n")
test_kruskal_lum <- kruskal.test(Nb_victimes ~ lum_label, data = accidents_modele %>% filter(!is.na(lum_label)))
print(test_kruskal_lum)

# ------------------------------------------------------------------------------
# 3.1.5 Type de collision vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.1.5 Type de collision x Nb_victimes ===\n")

biv_col <- accidents_modele %>%
  filter(!is.na(col_regroup)) %>%
  group_by(col_regroup) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  ) %>%
  arrange(desc(Moyenne_victimes))

print("Statistiques descriptives par type de collision regroupé :")
print(biv_col)

cat("\n* Test non-paramétrique de Kruskal-Wallis :\n")
print(kruskal.test(Nb_victimes ~ col_regroup, data = accidents_modele))

ggplot(biv_col, aes(x = reorder(col_regroup, Moyenne_victimes), y = Moyenne_victimes, fill = col_regroup)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne_victimes, " vict.")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2.4)) +
  labs(
    title = "Nombre moyen de victimes par accident selon le type de collision",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.1.6 Vitesse Maximale Autorisée (VMA) vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.1.6 Vitesse maximale autorisée x Nb_victimes ===\n")

biv_vma <- accidents_modele %>%
  mutate(tranche_vma = case_when(
    vma <= 30          ~ "<= 30 km/h (Zones 30)",
    vma == 50          ~ "50 km/h (Ville)",
    vma %in% c(70, 80) ~ "70-80 km/h (Secondaire)",
    vma == 90          ~ "90 km/h (Nationales)",
    vma >= 110         ~ ">= 110 km/h (Autoroutes)",
    TRUE               ~ "Autre"
  )) %>%
  group_by(tranche_vma) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 2),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 2)
  ) %>%
  arrange(Moyenne_victimes)

print("Statistiques descriptives par tranche de vitesse :")
print(biv_vma)

cat("\n* Test non-paramétrique de Kruskal-Wallis :\n")
print(kruskal.test(Nb_victimes ~ factor(vma), data = accidents_modele))

ggplot(biv_vma, aes(x = reorder(tranche_vma, Moyenne_victimes), y = Moyenne_victimes, fill = tranche_vma)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne_victimes, " vict.")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.85)) +
  labs(
    title = "Nombre moyen de victimes selon la vitesse maximale autorisée",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.1.7 Profil des usagers vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.1.7 Profil des usagers x Nb_victimes ===\n")

# A. Implication de jeunes conducteurs/usagers (18-24 ans)
cat("\n* Implication de jeunes (18-24 ans) vs Nb_victimes :\n")
stats_jeune <- accidents_modele %>%
  mutate(jeune_label = if_else(implique_jeune_18_24 == 1, "Avec jeune (18-24)", "Sans jeune (18-24)")) %>%
  group_by(jeune_label) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  )
print(stats_jeune)
cat("Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(Nb_victimes ~ implique_jeune_18_24, data = accidents_modele))

# B. Implication de seniors (65 ans et plus)
cat("\n* Implication de seniors (65+ ans) vs Nb_victimes :\n")
stats_senior <- accidents_modele %>%
  mutate(senior_label = if_else(implique_senior_65plus == 1, "Avec senior (65+)", "Sans senior (65+)")) %>%
  group_by(senior_label) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  )
print(stats_senior)
cat("Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(Nb_victimes ~ implique_senior_65plus, data = accidents_modele))

# C. Genre du conducteur
cat("\n* Genre du conducteur vs Nb_victimes :\n")
stats_sexe <- accidents_modele %>%
  mutate(
    sexe_label = if_else(au_moins_un_conducteur_homme == 1,
                              "Au moins un conducteur masculin",
                              "Aucun conducteur masculin recensé")
    ) %>%
  group_by(sexe_label) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  )
print(stats_sexe)
cat("Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(Nb_victimes ~ au_moins_un_conducteur_homme, data = accidents_modele))

# D. Non-port documenté de ceinture ou de casque
cat("\n* Non-port documenté de ceinture/casque vs Nb_victimes :\n")
stats_nonport <- accidents_modele %>%
  mutate(nonport_label = if_else(non_port_ceinture_casque == 1,
                                 "Non-port documenté",
                                 "Aucun non-port documenté")) %>%
  group_by(nonport_label) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  )
print(stats_nonport)
cat("Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(Nb_victimes ~ non_port_ceinture_casque, data = accidents_modele))

# ------------------------------------------------------------------------------
# 3.1.8 Obstacles et état de la chaussée vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.1.8 Obstacles et état de la chaussée x Nb_victimes ===\n")

# A. Choc contre obstacle fixe lourd
cat("\n* Choc contre obstacle fixe vs Nb_victimes :\n")
stats_obs <- accidents_modele %>%
  mutate(obs_label = if_else(choc_obstacle_fixe == 1, "Avec obstacle fixe", "Sans obstacle fixe")) %>%
  group_by(obs_label) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  )
print(stats_obs)
cat("Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(Nb_victimes ~ choc_obstacle_fixe, data = accidents_modele))

# B. État de la chaussée (surf_regroup)
cat("\n* État de la chaussée (surf_regroup) vs Nb_victimes :\n")
stats_surf <- accidents_modele %>%
  group_by(surf_regroup) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  ) %>%
  arrange(desc(Moyenne_victimes))
print(stats_surf)
cat("Test non-paramétrique de Kruskal-Wallis :\n")
print(kruskal.test(Nb_victimes ~ surf_regroup, data = accidents_modele))

cat("\n* Comparaison binaire : Chaussée glissante (mouillée/critique) vs Normale (Wilcoxon) :\n")
print(wilcox.test(Nb_victimes ~ I(surf_regroup %in% c("Mouillee / Inondee", "Glissante critique")), data = accidents_modele))


# ==============================================================================
# 3.2 Liaisons et Dépendances entre Variables Explicatives (X1 x X2)
# ==============================================================================

# Fonction utilitaire pour le calcul du V de Cramer (intensité de liaison Quali x Quali)
calc_cramer_v <- function(tab_contingence, test_chi2) {
  n <- sum(tab_contingence)
  k <- min(nrow(tab_contingence) - 1, ncol(tab_contingence) - 1)
  if (k < 1) return(NA)
  v <- sqrt(unname(test_chi2$statistic) / (n * k))
  qualif <- case_when(
    v < 0.10 ~ "Liaison très faible / négligeable",
    v < 0.30 ~ "Liaison faible à modérée",
    TRUE     ~ "Liaison forte"
  )
  cat(sprintf("* V de Cramer : %.4f (%s)\n", v, qualif))
  return(invisible(v))
}

# ------------------------------------------------------------------------------
# 3.2.1 Saison x Conditions atmosphériques (Quali x Quali)
# ------------------------------------------------------------------------------
cat("\n=== 3.2.1 Saison x Conditions atmosphériques ===\n")

tab_saison_atm <- table(accidents_modele$saison, accidents_modele$atm_label)
cat("Tableau de contingence (effectifs observés) :\n")
print(tab_saison_atm)

cat("\nProfils lignes (% au sein de chaque saison) :\n")
print(round(prop.table(tab_saison_atm, margin = 1) * 100, 1))

test_chi2_saison_atm <- chisq.test(tab_saison_atm)
print(test_chi2_saison_atm)
cat("Effectif théorique minimal :", round(min(test_chi2_saison_atm$expected), 2), 
    "(Validité >= 5 vérifiée ?", min(test_chi2_saison_atm$expected) >= 5, ")\n")
calc_cramer_v(tab_saison_atm, test_chi2_saison_atm)

ggplot(accidents_modele %>% filter(!is.na(saison), !is.na(atm_label)),
       aes(x = saison, fill = atm_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Répartition des conditions atmosphériques selon la saison",
    x = "Saison", y = "Proportion", fill = "Météo"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.2.2 Luminosité x Agglomération 
# ------------------------------------------------------------------------------
cat("\n=== 3.2.2 Luminosité x Agglomération ===\n")

tab_lum_agg <- table(accidents_modele$lum_label, accidents_modele$agg_label)
cat("Profils colonnes (% au sein de chaque zone) :\n")
print(round(prop.table(tab_lum_agg, margin = 2) * 100, 1))

test_chi2_lum_agg <- chisq.test(tab_lum_agg)
print(test_chi2_lum_agg)
calc_cramer_v(tab_lum_agg, test_chi2_lum_agg)

ggplot(accidents_modele %>% filter(!is.na(lum_label), !is.na(agg_label)),
       aes(x = agg_label, fill = lum_label)) +
  geom_bar(position = "fill", width = 0.55) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "Conditions de luminosité selon la localisation",
    x = "", y = "Proportion", fill = "Luminosité"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# ------------------------------------------------------------------------------
# 3.2.3 Conditions atmosphériques x Agglomération
# ------------------------------------------------------------------------------
cat("\n=== 3.2.3 Conditions atmosphériques x Agglomération ===\n")

tab_atm_agg <- table(accidents_modele$atm_label, accidents_modele$agg_label)
cat("Profils colonnes (% au sein de chaque zone) :\n")
print(round(prop.table(tab_atm_agg, margin = 2) * 100, 1))

test_chi2_atm_agg <- chisq.test(tab_atm_agg)
print(test_chi2_atm_agg)
calc_cramer_v(tab_atm_agg, test_chi2_atm_agg)

ggplot(accidents_modele %>% filter(!is.na(atm_label), !is.na(agg_label)),
       aes(x = agg_label, fill = atm_label)) +
  geom_bar(position = "fill", width = 0.55) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Conditions météorologiques selon la localisation",
    x = "", y = "Proportion", fill = "Météo"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# ------------------------------------------------------------------------------
# 3.2.4 Présence de piétons x Agglomération
# ------------------------------------------------------------------------------
cat("\n=== 3.2.4 Implication de piétons x Agglomération ===\n")

tab_pieton_agg <- table(
  factor(accidents_modele$implique_pieton, levels = c(0, 1), labels = c("Sans piéton", "Avec piéton")),
  accidents_modele$agg_label
)
cat("Profils colonnes (% d'accidents avec piétons par zone) :\n")
print(round(prop.table(tab_pieton_agg, margin = 2) * 100, 1))

test_chi2_pieton_agg <- chisq.test(tab_pieton_agg)
print(test_chi2_pieton_agg)
calc_cramer_v(tab_pieton_agg, test_chi2_pieton_agg)

ggplot(accidents_modele %>% filter(!is.na(agg_label)),
       aes(x = agg_label, fill = factor(implique_pieton, levels = c(1, 0), labels = c("Avec piéton", "Sans piéton")))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Avec piéton" = "#E53935", "Sans piéton" = "#B0BEC5")) +
  labs(
    title = "Présence de piétons selon la localisation",
    x = "", y = "Proportion", fill = "Statut piéton"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ------------------------------------------------------------------------------
# 3.2.5 Implication de poids lourds x Type de collision
# ------------------------------------------------------------------------------
cat("\n=== 3.2.5 Implication de poids lourds x Type de collision ===\n")

tab_pl_col <- table(
  factor(accidents_modele$implique_poids_lourd, levels = c(1, 0), labels = c("Avec poids lourd", "Sans poids lourd")),
  accidents_modele$col_regroup
)
cat("Profils lignes (% des types de collision selon présence PL) :\n")
print(round(prop.table(tab_pl_col, margin = 1) * 100, 1))

test_chi2_pl_col <- chisq.test(tab_pl_col)
print(test_chi2_pl_col)
calc_cramer_v(tab_pl_col, test_chi2_pl_col)

ggplot(accidents_modele %>% filter(!is.na(col_regroup)),
       aes(x = factor(implique_poids_lourd, levels = c(1, 0), labels = c("Avec poids lourd", "Sans poids lourd")), fill = col_regroup)) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Typologie de collision selon l'implication d'un poids lourd",
    x = "", y = "Proportion", fill = "Collision"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ------------------------------------------------------------------------------
# 3.2.6 Non-port documenté de ceinture/casque x Agglomération
# ------------------------------------------------------------------------------
cat("\n=== 3.2.6 Non-port documenté de ceinture/casque x Agglomération ===\n")

# 1 = au moins un usager concerné (voiture, utilitaire, poids lourd, deux-roues
# motorisé) sans équipement enregistré. 0 = aucun non-port documenté (≠ « tous équipés »).
tab_secu_agg <- table(
  factor(accidents_modele$non_port_ceinture_casque, levels = c(1, 0),
         labels = c("Non-port documenté", "Aucun non-port documenté")),
  accidents_modele$agg_label
)
cat("Profils colonnes (% d'accidents avec non-port documenté, par zone) :\n")
print(round(prop.table(tab_secu_agg, margin = 2) * 100, 1))
cat("Accidents dont l'équipement n'est pas évaluable :",
    sum(accidents_modele$equipement_evaluable == 0), "\n")

test_chi2_secu_agg <- chisq.test(tab_secu_agg)
print(test_chi2_secu_agg)
calc_cramer_v(tab_secu_agg, test_chi2_secu_agg)

ggplot(accidents_modele %>% filter(!is.na(agg_label)),
       aes(x = agg_label, fill = factor(non_port_ceinture_casque, levels = c(1, 0),
                                        labels = c("Non-port documenté", "Aucun non-port documenté")))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Non-port documenté" = "#FB8C00",
                               "Aucun non-port documenté" = "#78909C")) +
  labs(
    title = "Non-port documenté de ceinture/casque selon la zone",
    x = "", y = "Proportion", fill = "Équipement"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
# ------------------------------------------------------------------------------
# 3.2.7 Vitesse (VMA) x Agglomération
# ------------------------------------------------------------------------------
cat("\n=== 3.2.7 Vitesse maximale autorisée x Agglomération ===\n")

stats_vma_agg <- accidents_modele %>%
  group_by(agg_label) %>%
  summarise(
    Effectif   = n(),
    Moyenne    = round(mean(vma, na.rm = TRUE), 1),
    Mediane    = median(vma, na.rm = TRUE),
    Ecart_type = round(sd(vma, na.rm = TRUE), 1),
    Q1         = quantile(vma, 0.25, na.rm = TRUE),
    Q3         = quantile(vma, 0.75, na.rm = TRUE)
  )
print("Statistiques descriptives de la VMA par zone :")
print(stats_vma_agg)

cat("\n* Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(vma ~ agg_label, data = accidents_modele))

ggplot(accidents_modele %>% filter(!is.na(agg_label), !is.na(vma)),
       aes(x = agg_label, y = vma, fill = agg_label)) +
  geom_boxplot(width = 0.4, outlier.alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = c("Hors agglomération" = "#5C6BC0", "En agglomération" = "#26A69A")) +
  labs(
    title = "Distribution de la vitesse maximale autorisée (VMA) par zone",
    caption = "Le losange blanc représente la moyenne",
    x = "", y = "VMA (km/h)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# ==============================================================================
# 3.3 Croisement Quantitative x Quantitative
# ==============================================================================

cat("\n=======================================================\n")
cat("  3.3 CROISEMENT QUANTITATIVE x QUANTITATIVE           \n")
cat("=======================================================\n")

# ------------------------------------------------------------------------------
# 3.3.1 Vitesse Maximale Autorisée (VMA continue) vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.3.1 VMA continue x Nb_victimes ===\n")

cor_vma_pearson <- cor.test(accidents_modele$vma, accidents_modele$Nb_victimes, method = "pearson")
cat("* Test de corrélation linéaire de Pearson (H0: R = 0 vs H1: R != 0) :\n")
print(cor_vma_pearson)

cor_vma_spearman <- cor.test(accidents_modele$vma, accidents_modele$Nb_victimes, method = "spearman")
cat("\n* Test de corrélation de rang de Spearman (H0: Rs = 0 vs H1: Rs != 0) :\n")
print(cor_vma_spearman)

ggplot(accidents_modele, aes(x = vma, y = Nb_victimes)) +
  geom_jitter(width = 2, height = 0.25, alpha = 0.08, color = "#1E88E5") +
  geom_smooth(method = "lm", color = "#D32F2F", se = TRUE, linewidth = 1.1) +
  scale_x_continuous(breaks = c(30, 50, 70, 80, 90, 110, 130)) +
  scale_y_continuous(breaks = seq(1, 15, by = 2), limits = c(0.8, 16)) +
  labs(
    title = "Relation entre la vitesse autorisée (VMA) et le nombre de victimes",
    x = "Vitesse Maximale Autorisée (km/h)",
    y = "Nombre de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.3.2 Âge moyen des usagers vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.3.2 Âge moyen des usagers x Nb_victimes ===\n")

cor_age_pearson <- cor.test(accidents_modele$age_moyen_usagers, accidents_modele$Nb_victimes, method = "pearson")
cat("* Test de corrélation linéaire de Pearson :\n")
print(cor_age_pearson)

cor_age_spearman <- cor.test(accidents_modele$age_moyen_usagers, accidents_modele$Nb_victimes, method = "spearman")
cat("\n* Test de corrélation de rang de Spearman :\n")
print(cor_age_spearman)

ggplot(accidents_modele, aes(x = age_moyen_usagers, y = Nb_victimes)) +
  geom_jitter(width = 0.5, height = 0.25, alpha = 0.08, color = "#43A047") +
  geom_smooth(method = "lm", color = "#D32F2F", se = TRUE, linewidth = 1.1) +
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(1, 15, by = 2), limits = c(0.8, 16)) +
  labs(
    title = "Relation entre l'âge moyen des usagers et le nombre de victimes",
    x = "Âge moyen des usagers impliqués (années)",
    y = "Nombre de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.3.3 Nombre de véhicules impliqués vs Nb_victimes
# ------------------------------------------------------------------------------
cat("\n=== 3.3.3 Nombre de véhicules impliqués x Nb_victimes ===\n")

cor_veh_pearson <- cor.test(accidents_modele$nb_vehicules_impliquees, accidents_modele$Nb_victimes, method = "pearson")
cat("* Test de corrélation linéaire de Pearson :\n")
print(cor_veh_pearson)

cor_veh_spearman <- cor.test(accidents_modele$nb_vehicules_impliquees, accidents_modele$Nb_victimes, method = "spearman")
cat("\n* Test de corrélation de rang de Spearman :\n")
print(cor_veh_spearman)

# Graphique de la relation Nombre de véhicules vs Nombre de victimes
ggplot(accidents_modele, aes(x = nb_vehicules_impliquees, y = Nb_victimes)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.08, color = "#8E24AA") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1) +
  scale_x_continuous(breaks = 1:11) +
  labs(
    title = "Relation entre le nombre de véhicules impliqués et le nombre de victimes",
    x = "Nombre de véhicules impliqués dans l'accident",
    y = "Nombre de victimes par accident"
  ) +
  theme_minimal(base_size = 12)


# ==============================================================================
# SECTION 4 : MODÉLISATION LINÉAIRE MULTIPLE ET SÉLECTION DE VARIABLES
# ==============================================================================

cat("\n=======================================================\n")
cat("  SECTION 4 : MODÉLISATION LINÉAIRE ET SÉLECTION DE VARIABLES\n")
cat("=======================================================\n")

train <- subset(accidents_modele, echantillon == "train")
test  <- subset(accidents_modele, echantillon == "test")
cat("Train :", nrow(train), "| Test :", nrow(test), "\n")

# ------------------------------------------------------------------------------
# 4.1 Estimation du modèle complet
# ------------------------------------------------------------------------------
mod_complet <- lm(
  Nb_victimes ~ saison + atm_label + agg_label + lum_label + col_regroup +
    vma + nb_vehicules_impliquees + nb_usagers + implique_pieton +
    implique_jeune_18_24 + implique_senior_65plus + choc_obstacle_fixe +
    non_port_ceinture_casque +
    implique_voiture + implique_moto + implique_velo_edp +
    implique_poids_lourd + implique_transp_commun,
  data = train
)

cat("\n=== 4.1 Résumé du modèle linéaire multiple complet ===\n")
print(summary(mod_complet))
cat("AIC du modèle complet :", round(AIC(mod_complet), 2), "\n")

# ------------------------------------------------------------------------------
# 4.2 Procédure de sélection de variables
# ------------------------------------------------------------------------------
cat("\n=== 4.2 Procédure de sélection de variables ===\n")
mod_optimal <- stepAIC(mod_complet, direction = "both", trace = TRUE)

# stepAIC affiche extractAIC (sans constante), AIC() la formule complète.
# Les valeurs diffèrent d'une constante, mais le ΔAIC entre modèles est identique.

cat("\n=== Modèle optimal retenu par AIC ===\n")
print(summary(mod_optimal))
cat(sprintf("AIC initial : %.2f --> AIC optimal : %.2f\n", AIC(mod_complet), AIC(mod_optimal)))

# ------------------------------------------------------------------------------
# 4.3 Diagnostic graphique des résidus
# ------------------------------------------------------------------------------
cat("\n=== 4.3 Diagnostic des résidus ===\n")
residus <- residuals(mod_optimal)
valeurs_ajustees <- fitted(mod_optimal)

cat("Distribution des résidus :\n")
print(summary(residus))

ggplot(data.frame(Fitted = valeurs_ajustees, Residuals = residus), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.08, color = "#546E7A") +
  geom_hline(yintercept = 0, color = "#D32F2F", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", color = "#1E88E5", se = FALSE) +
  labs(
    title = "Diagnostic des résidus du modèle linéaire",
    x = "Valeurs prédites",
    y = "Résidus"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 4.4 Vérification des conditions de validité
# ------------------------------------------------------------------------------
# Résidus standardisés
res_std <- rstandard(mod_optimal)

# Normalité des résidus : QQ-plot, histogramme, test de Kolmogorov-Smirnov (n > 50)
par(mfrow = c(1, 2))
plot(mod_optimal, which = 2)   # QQ-plot
hist(res_std, breaks = 50, main = "Histogramme des résidus standardisés",
     xlab = "Valeur", ylab = "Fréquence")
par(mfrow = c(1, 1))
ks.test(res_std, "pnorm", mean(res_std), sd(res_std))

# Variance constante (homoscédasticité) : graphique puis test de Breusch-Pagan
plot(mod_optimal, which = 3)
bptest(mod_optimal)

# Linéarité : résidus en fonction des valeurs ajustées
plot(mod_optimal, which = 1)

# Observations aberrantes, leviers et influentes
cook <- cooks.distance(mod_optimal)
cat("Distance de Cook maximale :", round(max(cook), 3), "\n")
cat("Observations avec Cook > 1 :", sum(cook > 1), "\n")

levier <- hatvalues(mod_optimal)
p <- length(coef(mod_optimal))   # nombre de paramètres, constante comprise
seuil_levier <- 2 * p / nrow(train)
cat("Points leviers (levier >", round(seuil_levier, 4), ") :", sum(levier > seuil_levier), "\n")


# ------------------------------------------------------------------------------
# 4.5 Transformation de Y : Recherche du lambda de Box-Cox
# ------------------------------------------------------------------------------
# Box-Cox teste Y^lambda : lambda = 1 (rien), 0.5 (racine), 0 (log), -1 (inverse), 2(carre)
# Il faut Y > 0, ce qui est vrai ici (Nb_victimes >= 1)
bc <- boxcox(mod_optimal, lambda = seq(-2, 2, by = 0.1))   # MASS est déjà chargé

lambda_opt <- bc$x[which.max(bc$y)]
cat("Lambda optimal :", lambda_opt, "\n")


# ------------------------------------------------------------------------------
# 4.6 Transformations racine et log : RMSE et MAE sur le jeu de test
# ------------------------------------------------------------------------------
# On garde les mêmes variables explicatives que mod_optimal
mod_sqrt <- update(mod_optimal, sqrt(Nb_victimes) ~ .)
mod_log  <- update(mod_optimal, log(Nb_victimes) ~ .)

# Les conditions de validité sont-elles meilleures après transformation ?
bptest(mod_sqrt)
bptest(mod_log)
ks.test(rstandard(mod_sqrt), "pnorm", mean(rstandard(mod_sqrt)), sd(rstandard(mod_sqrt)))
ks.test(rstandard(mod_log),  "pnorm", mean(rstandard(mod_log)),  sd(rstandard(mod_log)))

# Prédictions sur le jeu de TEST, ramenées en nombre de victimes
pred_moy  <- rep(mean(train$Nb_victimes), nrow(test))   # référence : la moyenne seule
pred_brut <- predict(mod_optimal, newdata = test)
pred_sqrt <- predict(mod_sqrt, newdata = test)^2        # on annule la racine
pred_log  <- exp(predict(mod_log, newdata = test))      # on annule le log

# Critères de prédiction
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))

resultats <- data.frame(
  Modele = c("Moyenne seule", "Sans transformation", "Racine carrée", "Log"),
  RMSE = c(rmse(test$Nb_victimes, pred_moy),  rmse(test$Nb_victimes, pred_brut),
           rmse(test$Nb_victimes, pred_sqrt), rmse(test$Nb_victimes, pred_log)),
  MAE  = c(mae(test$Nb_victimes, pred_moy),  mae(test$Nb_victimes, pred_brut),
           mae(test$Nb_victimes, pred_sqrt), mae(test$Nb_victimes, pred_log))
)
resultats$RMSE <- round(resultats$RMSE, 4)
resultats$MAE  <- round(resultats$MAE, 4)
print(resultats)


# ------------------------------------------------------------------------------
# 4.7 Erreurs standard non sensibles à l'hétéroscédasticité HC3
# ------------------------------------------------------------------------------
# Les coefficients ne changent pas : seuls les écarts-types, les t et les p changent
# coeftest permet de tester les coefficients avec cette matrice

coef_classique <- coeftest(mod_optimal)
coef_robuste   <- coeftest(mod_optimal, vcov = vcovHC(mod_optimal, type = "HC3"))

comparaison <- data.frame(
  t_classique = round(coef_classique[, "t value"], 2),
  t_robuste   = round(coef_robuste[, "t value"], 2),
  p_robuste   = signif(coef_robuste[, "Pr(>|t|)"], 3)
)
print(comparaison)


# ------------------------------------------------------------------------------
# 4.8 Sensibilité à la variable nb_usagers
# ------------------------------------------------------------------------------
# On compare deux modèles COMPLETS (mêmes variables), avec et sans nb_usagers
mod_complet_sans <- update(mod_complet, . ~ . - nb_usagers)

variables <- c("saisonEte", "implique_jeune_18_24", "implique_senior_65plus",
               "non_port_ceinture_casque", "nb_vehicules_impliquees")
sensibilite <- data.frame(
  sans_nb_usagers = round(coef(mod_complet_sans)[variables], 3),
  avec_nb_usagers = round(coef(mod_complet)[variables], 3)
)
print(sensibilite)

# Même comparaison sur le jeu de test
cat("RMSE test sans nb_usagers :",
    round(rmse(test$Nb_victimes, predict(mod_complet_sans, newdata = test)), 4), "\n")
cat("RMSE test avec nb_usagers :",
    round(rmse(test$Nb_victimes, predict(mod_complet, newdata = test)), 4), "\n")
  

# ------------------------------------------------------------------------------
# 4.9 Observations influentes (Cook > 1)
# ------------------------------------------------------------------------------
obs_influentes <- which(cook > 1)
print(train[obs_influentes, c("Num_Acc", "Nb_victimes", "nb_usagers",
                        "nb_vehicules_impliquees", "col_regroup", "agg_label")])

# Même modèle, ajusté sans ces observations
train_sans <- train[-obs_influentes, ]
mod_sans_infl <- update(mod_optimal, data = train_sans)

# Coefficients avec et sans ces observations
comparaison_infl <- data.frame(
  avec = round(coef(mod_optimal), 3),
  sans = round(coef(mod_sans_infl), 3)
)
print(comparaison_infl)

# Performance sur le même jeu de test
cat("RMSE test, modèle complet    :",
    round(rmse(test$Nb_victimes, predict(mod_optimal, newdata = test)), 4), "\n")
cat("RMSE test, sans ces points influents :",
    round(rmse(test$Nb_victimes, predict(mod_sans_infl, newdata = test)), 4), "\n")


# ------------------------------------------------------------------------------
# 4.10 Test direct de la saison et de la météo
# ------------------------------------------------------------------------------
# drop1 retire chaque variable du modèle
# et teste si le modèle devient significativement moins bon.

# H0 : la variable n'a aucun effet     H1 : elle a un effet
drop1(mod_complet, test = "F")

# Même lecture avec les erreurs HC3 (comme au bloc 4.7) : on regarde les lignes
# saison... et atm_label...
coeftest(mod_complet, vcov = vcovHC(mod_complet, type = "HC3"))

# Test global avec erreurs robustes (HC3) : saison puis météo
waldtest(update(mod_complet, . ~ . - saison), mod_complet, test = "Chisq",
         vcov = function(x) vcovHC(x, type = "HC3"))
waldtest(update(mod_complet, . ~ . - atm_label), mod_complet, test = "Chisq",
         vcov = function(x) vcovHC(x, type = "HC3"))


# ==============================================================================
# SECTION 5 : ANALYSE STATISTIQUE SPATIALE (INDICES DE MORAN ET GEARY)
# ==============================================================================

cat("\n=======================================================\n")
cat("      SECTION 5 : ANALYSE STATISTIQUE SPATIALE         \n")
cat("=======================================================\n")

# ------------------------------------------------------------------------------
# 5.1 Tableau départemental : victimes, usagers, résidus du modèle
# ------------------------------------------------------------------------------
# Résidu = réalité - prédiction du modèle (ce que le modèle n'explique pas)
accidents_modele$residu <- accidents_modele$Nb_victimes -
  predict(mod_optimal, newdata = accidents_modele)

dep_stats <- accidents_modele %>%
  group_by(dep, nom_dep) %>%
  summarise(
    n_accidents  = n(),
    victimes_moy = mean(Nb_victimes),
    usagers_moy  = mean(nb_usagers),
    residu_moy   = mean(residu),
    lat          = mean(lat),
    long         = mean(long),
    .groups      = "drop"
  )

print(head(dep_stats))
cat("Nombre de départements :", nrow(dep_stats), "\n")


# ------------------------------------------------------------------------------
# 5.2 Voisinage par k plus proches voisins
# ------------------------------------------------------------------------------
# Coordonnées de chaque département (longitude, latitude)
coords <- cbind(dep_stats$long, dep_stats$lat)

# Chaque département a pour voisins les 5 départements les plus proches
voisins_k5 <- knn2nb(knearneigh(coords, k = 5, longlat = TRUE))

print(voisins_k5)            # résumé : nombre de liens, voisins par département
plot(voisins_k5, coords)     # graphe de voisinage


# ------------------------------------------------------------------------------
# 5.3 Moran et Geary (k = 5) et sensibilité au nombre de voisins (k = 3, 5, 8)
# ------------------------------------------------------------------------------
# 5.3a Test de dépendance spatiale du nombre moyen de victimes (k = 5)
# Matrice de poids : chaque ligne somme à 1 (style "W")
poids_k5 <- nb2listw(voisins_k5, style = "W")

# H0 : pas d'autocorrélation spatiale   H1 : autocorrélation spatiale
moran.test(dep_stats$victimes_moy, listw = poids_k5, alternative = "two.sided")
geary.test(dep_stats$victimes_moy, listw = poids_k5, alternative = "two.sided")

# 5.3b Sensibilité au choix du nombre de voisins
voisins_k3 <- knn2nb(knearneigh(coords, k = 3, longlat = TRUE))
voisins_k8 <- knn2nb(knearneigh(coords, k = 8, longlat = TRUE))
poids_k3 <- nb2listw(voisins_k3, style = "W")
poids_k8 <- nb2listw(voisins_k8, style = "W")

moran_k3 <- moran.test(dep_stats$victimes_moy, poids_k3, alternative = "two.sided")
moran_k5 <- moran.test(dep_stats$victimes_moy, poids_k5, alternative = "two.sided")
moran_k8 <- moran.test(dep_stats$victimes_moy, poids_k8, alternative = "two.sided")
geary_k3 <- geary.test(dep_stats$victimes_moy, poids_k3, alternative = "two.sided")
geary_k5 <- geary.test(dep_stats$victimes_moy, poids_k5, alternative = "two.sided")
geary_k8 <- geary.test(dep_stats$victimes_moy, poids_k8, alternative = "two.sided")

sensibilite_k <- data.frame(
  k       = c(3, 5, 8),
  Moran_I = round(c(moran_k3$estimate[1], moran_k5$estimate[1], moran_k8$estimate[1]), 3),
  p_Moran = signif(c(moran_k3$p.value, moran_k5$p.value, moran_k8$p.value), 3),
  Geary_C = round(c(geary_k3$estimate[1], geary_k5$estimate[1], geary_k8$estimate[1]), 3),
  p_Geary = signif(c(geary_k3$p.value, geary_k5$p.value, geary_k8$p.value), 3)
)
print(sensibilite_k)


# ------------------------------------------------------------------------------
# 5.4 Dépendance spatiale des résidus du modèle et du nombre d'usagers
# ------------------------------------------------------------------------------
# H0 : pas d'autocorrélation spatiale   H1 : autocorrélation spatiale

# Résidus moyens par département, pour k = 3, 5 et 8
moran_res_k3 <- moran.test(dep_stats$residu_moy, poids_k3, alternative = "two.sided")
moran_res_k5 <- moran.test(dep_stats$residu_moy, poids_k5, alternative = "two.sided")
moran_res_k8 <- moran.test(dep_stats$residu_moy, poids_k8, alternative = "two.sided")

tableau_residus <- data.frame(
  k       = c(3, 5, 8),
  Moran_I = round(c(moran_res_k3$estimate[1], moran_res_k5$estimate[1],
                    moran_res_k8$estimate[1]), 3),
  p_Moran = signif(c(moran_res_k3$p.value, moran_res_k5$p.value,
                     moran_res_k8$p.value), 3)
)
print(tableau_residus)

# Nombre moyen d'usagers par accident (k = 5)
moran.test(dep_stats$usagers_moy, poids_k5, alternative = "two.sided")


# ------------------------------------------------------------------------------
# 5.5 Contours des départements et voisinage par frontières communes
# ------------------------------------------------------------------------------
# Lecture du fichier de contours (96 départements, Corse comprise)
fr_dep <- st_read("Ressources/departements-version-simplifiee.geojson")

# On relie les contours à notre tableau par le code du département.
# merge ne garde que les départements présents des deux côtés : la Corse disparaît
fr_dep <- merge(fr_dep, dep_stats, by.x = "code", by.y = "dep")
cat("Départements après jointure :", nrow(fr_dep), "\n")

# Voisins = départements qui partagent une frontière (queen = TRUE, comme le cours)
voisins_contig <- poly2nb(fr_dep, queen = TRUE)
print(voisins_contig)
plot(st_geometry(fr_dep), border = "gray")
plot(voisins_contig, st_geometry(fr_dep), add = TRUE)


# ------------------------------------------------------------------------------
# 5.6 Moran et Geary avec les frontières communes (contiguïté queen)
# ------------------------------------------------------------------------------
# H0 : pas d'autocorrélation spatiale   H1 : autocorrélation spatiale
poids_contig <- nb2listw(voisins_contig, style = "W")

variables_spatiales <- c("victimes_moy", "usagers_moy", "residu_moy")

resultats_contig <- data.frame(
  Variable = variables_spatiales,
  Moran_I = NA, p_Moran = NA, Geary_C = NA, p_Geary = NA
)

for (i in 1:3) {
  x <- fr_dep[[variables_spatiales[i]]]    # la colonne à tester
  m <- moran.test(x, poids_contig, alternative = "two.sided")
  g <- geary.test(x, poids_contig, alternative = "two.sided")
  resultats_contig$Moran_I[i] <- round(m$estimate[1], 3)
  resultats_contig$p_Moran[i] <- signif(m$p.value, 3)
  resultats_contig$Geary_C[i] <- round(g$estimate[1], 3)
  resultats_contig$p_Geary[i] <- signif(g$p.value, 3)
}
print(resultats_contig)


# ------------------------------------------------------------------------------
# 5.7 Cartes en aplats de couleurs et synthèse départementale
# ------------------------------------------------------------------------------
# Carte 1 : nombre moyen de victimes par accident
ggplot(data = fr_dep, aes(fill = victimes_moy)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr") +
  labs(title = "Nombre moyen de victimes par accident",
       fill = "Victimes\nmoyennes") +
  theme_minimal()

# Carte 2 : résidu moyen du modèle (blanc = le modèle ne se trompe pas en moyenne)
ggplot(data = fr_dep, aes(fill = residu_moy)) +
  geom_sf() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr") +
  labs(title = "Résidu moyen du modèle linéaire",
       fill = "Résidu\nmoyen") +
  theme_minimal()

# Les 5 départements aux valeurs les plus hautes et les plus basses
head(arrange(dep_stats, desc(victimes_moy)), 5)
head(arrange(dep_stats, victimes_moy), 5)


cat("\n=======================================================\n")
cat("          FIN DE L'EXÉCUTION DU SCRIPT R              \n")
cat("=======================================================\n")





# ==============================================================================
# EXPORT DES FIGURES DU RAPPORT (régression et analyse spatiale)
# ==============================================================================
dir.create("Rapport/figures", showWarnings = FALSE, recursive = TRUE)

# Diagnostic du modèle : les 4 graphiques du cours (partie 3) avec titres et axes en français
png("Rapport/figures/diag_modele.png", width = 2400, height = 2000, res = 300)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1.5))
y_fit    <- fitted(mod_optimal)
res      <- residuals(mod_optimal)
res_std  <- rstandard(mod_optimal)
cook     <- cooks.distance(mod_optimal)
obs_ids  <- rownames(train)

# 1. Résidus vs Valeurs ajustées
plot(y_fit, res,
     xlab = "Valeurs ajustées (prédites)",
     ylab = "Résidus",
     main = "Résidus vs Valeurs ajustées",
     pch = 1, col = "black", cex = 0.8)
abline(h = 0, lty = 3, col = "gray50")
lines(lowess(y_fit, res), col = "red", lwd = 1.2)
top1 <- order(abs(res), decreasing = TRUE)[1:3]
text(y_fit[top1], res[top1], labels = obs_ids[top1], pos = c(4, 2, 2), cex = 0.75, col = "black")

# 2. QQ-plot normal
qq <- qqnorm(res_std,
             main = "Diagramme Q-Q normal",
             xlab = "Quantiles théoriques",
             ylab = "Résidus standardisés",
             pch = 1, col = "black", cex = 0.8)
qqline(res_std, lty = 3, col = "gray50")
top2 <- order(abs(res_std), decreasing = TRUE)[1:3]
text(qq$x[top2], qq$y[top2], labels = obs_ids[top2], pos = c(2, 4, 4), cex = 0.75, col = "black")

# 3. Échelle-Localisation
sqrt_abs <- sqrt(abs(res_std))
plot(y_fit, sqrt_abs,
     xlab = "Valeurs ajustées (prédites)",
     ylab = expression(sqrt(paste("|", "Résidus standardisés", "|"))),
     main = "Échelle-Localisation",
     pch = 1, col = "black", cex = 0.8)
lines(lowess(y_fit, sqrt_abs), col = "red", lwd = 1.2)
top3 <- order(sqrt_abs, decreasing = TRUE)[1:3]
text(y_fit[top3], sqrt_abs[top3], labels = obs_ids[top3], pos = c(2, 4, 2), cex = 0.75, col = "black")

# 4. Distance de Cook
plot(cook, type = "h",
     xlab = "Numéro d'observation",
     ylab = "Distance de Cook",
     main = "Distance de Cook",
     ylim = c(0, max(cook, na.rm = TRUE) * 1.1),
     col = "gray20")
top4 <- order(cook, decreasing = TRUE)[1:3]
text(top4, cook[top4], labels = obs_ids[top4], pos = 3, cex = 0.75, col = "black")

par(mfrow = c(1, 1))
dev.off()

# Courbe de Box-Cox
png("Rapport/figures/boxcox.png", width = 1800, height = 1400, res = 300)
boxcox(mod_optimal, lambda = seq(-2, 2, by = 0.1))
dev.off()

# Carte 1 : nombre moyen de victimes par accident (aplats de couleurs)
carte_victimes <- ggplot(fr_dep, aes(fill = victimes_moy)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr") +
  labs(fill = "Victimes\nmoyennes") +
  theme_minimal()
ggsave("Rapport/figures/carte_victimes.png", carte_victimes,
       width = 7, height = 6, dpi = 300)

# Carte 2 : résidu moyen du modèle
carte_residus <- ggplot(fr_dep, aes(fill = residu_moy)) +
  geom_sf() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr") +
  labs(fill = "Résidu\nmoyen") +
  theme_minimal()
ggsave("Rapport/figures/carte_residus.png", carte_residus,
       width = 7, height = 6, dpi = 300)