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
#   2.5 Variable Géographique : Taux d'accidentalité pour 100 000 habitants (Top 20)
#
# SECTION 3 : ANALYSES BIVARIÉES ET INFÉRENCE STATISTIQUE (CONFORME COURS PARTIE 2)
#   3.1 Facteurs Explicatifs vs Variable Cible (X x Y, Sévérité = Nb_victimes)
#       3.1.1 Saison vs Sévérité (Quali x Quanti)
#       3.1.2 Conditions atmosphériques vs Sévérité (Quali x Quanti)
#       3.1.3 Agglomération (Zone) vs Sévérité (Quali x Quanti, 2 groupes)
#       3.1.4 Luminosité vs Sévérité (Quali x Quanti)
#       3.1.5 Type de collision regroupé vs Sévérité (Quali x Quanti)
#       3.1.6 Vitesse Maximale Autorisée (VMA) vs Sévérité (Tranches de vitesse)
#       3.1.7 Facteurs usagers et démographiques vs Sévérité (Jeunes, Seniors, Hommes)
#       3.1.8 Obstacles et État de la chaussée vs Sévérité (Obstacle fixe, Surface)
#   3.2 Liaisons et Dépendances entre Variables Explicatives (X1 x X2, Quali x Quali)
#       3.2.1 Saison x Conditions atmosphériques (Chi-2 & V de Cramer)
#       3.2.2 Luminosité x Agglomération (Chi-2 & V de Cramer)
#       3.2.3 Conditions atmosphériques x Agglomération (Chi-2 & V de Cramer)
#       3.2.4 Présence de piétons x Agglomération (Chi-2 & V de Cramer)
#       3.2.5 Implication de poids lourds x Type de collision (Chi-2 & V de Cramer)
#       3.2.6 Défaut d'équipement de sécurité x Agglomération (Chi-2 & V de Cramer)
#       3.2.7 Vitesse (VMA) x Agglomération (Quanti x Quali)
#   3.3 Croisement Quantitative x Quantitative (Chapitre IV du cours)
#       3.3.1 VMA continue vs Sévérité (Corrélations de Pearson & Spearman)
#       3.3.2 Âge moyen des usagers vs Sévérité (Corrélations de Pearson & Spearman)
#       3.3.3 Nombre de véhicules impliqués vs Sévérité (Pearson & Spearman)
#
# SECTION 4 : ANALYSE STATISTIQUE SPATIALE (AUTOCORRÉLATION & INDICE DE MORAN)
#   4.1 Agrégation départementale métropolitaine et calcul des centroïdes
#   4.2 Matrice de pondération spatiale par inverse des distances
#   4.3 Test d'autocorrélation spatiale globale de Moran
#   4.4 Cartographie thématique de la sévérité départementale
#
# SECTION 5 : MODÉLISATION LINÉAIRE MULTIPLE ET SÉLECTION DE VARIABLES
#   5.1 Estimation du modèle linéaire complet
#   5.2 Sélection pas-à-pas de variables (stepAIC direction = 'both')
#   5.3 Audit des conditions de validité des résidus (Gauss-Markov)
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
library(ape)
library(maps)

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

# Typologie de collision (6 catégories validées empiriquement)
cat("\n* Typologie de collision (regroupement justifié par la sévérité) :\n")
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
cat("CADRE MÉTHODOLOGIQUE (CONFORME AU COURS DE C. FRÉVENT - PARTIE 2) :\n")
cat("1. Croisement Quanti x Quali  : Arbre décisionnel (K-S / Bartlett / Wilcoxon / Kruskal-Wallis)\n")
cat("2. Croisement Quali x Quali   : Tableaux de contingence, tests du Chi-2 et V de Cramer\n")
cat("3. Croisement Quanti x Quanti : Nuages de points et corrélations de Pearson & Spearman\n")

# ==============================================================================
# 3.1 Facteurs explicatifs vs Variable cible (X x Y, avec Y = Nb_victimes)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1.1 Saison vs Sévérité (Quali x Quanti)
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

# Graphique de la sévérité moyenne par saison
ggplot(stats_saison, aes(x = saison, y = Moyenne, fill = saison)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1.6)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Sévérité moyenne des accidents selon la saison",
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
# 3.1.2 Conditions atmosphériques (Météo) vs Sévérité
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

# Graphique de la sévérité moyenne par condition météorologique
ggplot(stats_atm, aes(x = reorder(atm_label, Moyenne), y = Moyenne, fill = atm_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            hjust = -0.1, size = 3.8, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.8)) +
  labs(
    title = "Sévérité moyenne selon les conditions atmosphériques",
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
# 3.1.3 Agglomération (Zone) vs Sévérité
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
print("Statistiques descriptives : Agglomération vs Sévérité :")
print(stats_agg)

# Graphique de la sévérité moyenne par zone
ggplot(stats_agg, aes(x = agg_label, y = Moyenne, fill = agg_label)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            vjust = -0.5, size = 4.2, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1.8)) +
  scale_fill_manual(values = c("Hors agglomération" = "#EF5350", "En agglomération" = "#42A5F5")) +
  labs(
    title = "Sévérité moyenne des accidents selon la zone",
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
# 3.1.4 Luminosité vs Sévérité (Quali x Quanti)
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
print("Statistiques descriptives : Luminosité vs Sévérité :")
print(stats_lum)

# Graphique de la sévérité moyenne par condition d'éclairage (ordonné)
ggplot(stats_lum, aes(x = reorder(lum_label, Moyenne), y = Moyenne, fill = lum_label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.7)) +
  labs(
    title = "Sévérité moyenne selon les conditions de luminosité",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

cat("\n* Test d'homoscédasticité de Bartlett :\n")
print(bartlett.test(Nb_victimes ~ lum_label, data = accidents_modele %>% filter(!is.na(lum_label))))

cat("\n* Test non-paramétrique de Kruskal-Wallis :\n")
test_kruskal_lum <- kruskal.test(Nb_victimes ~ lum_label, data = accidents_modele %>% filter(!is.na(lum_label)))
print(test_kruskal_lum)

# ------------------------------------------------------------------------------
# 3.1.5 Type de collision vs Sévérité
# ------------------------------------------------------------------------------
cat("\n=== 3.1.5 Type de collision x Nb_victimes ===\n")

biv_col <- accidents_modele %>%
  filter(!is.na(col_regroup)) %>%
  group_by(col_regroup) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 2),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 2)
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
    title = "Sévérité moyenne des accidents selon le type de collision",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.1.6 Vitesse Maximale Autorisée (VMA) vs Sévérité
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
    title = "Sévérité moyenne des accidents selon la vitesse maximale autorisée",
    x = "", y = "Nombre moyen de victimes par accident"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# 3.1.7 Facteurs démographiques et profil des usagers vs Sévérité
# ------------------------------------------------------------------------------
cat("\n=== 3.1.7 Facteurs usagers x Nb_victimes ===\n")

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
  mutate(sexe_label = if_else(conducteur_homme == 1, "Conducteur Homme", "Conductrice Femme")) %>%
  group_by(sexe_label) %>%
  summarise(
    Effectif         = n(),
    Moyenne_victimes = round(mean(Nb_victimes), 3),
    Mediane          = median(Nb_victimes),
    Ecart_type       = round(sd(Nb_victimes), 3)
  )
print(stats_sexe)
cat("Test non-paramétrique de Wilcoxon-Mann-Whitney :\n")
print(wilcox.test(Nb_victimes ~ conducteur_homme, data = accidents_modele))

# ------------------------------------------------------------------------------
# 3.1.8 Obstacles et État de la chaussée vs Sévérité
# ------------------------------------------------------------------------------
cat("\n=== 3.1.8 Obstacles et État de la chaussée x Nb_victimes ===\n")

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
# 3.2.6 Défaut d'équipement de sécurité x Agglomération
# ------------------------------------------------------------------------------
cat("\n=== 3.2.6 Défaut d'équipement de sécurité x Agglomération ===\n")

tab_secu_agg <- table(
  factor(accidents_modele$defaut_securite, levels = c(1, 0), labels = c("Défaut d'équipement", "Équipé conforme")),
  accidents_modele$agg_label
)
cat("Profils colonnes (% de défaut de sécurité par zone) :\n")
print(round(prop.table(tab_secu_agg, margin = 2) * 100, 1))

test_chi2_secu_agg <- chisq.test(tab_secu_agg)
print(test_chi2_secu_agg)
calc_cramer_v(tab_secu_agg, test_chi2_secu_agg)

ggplot(accidents_modele %>% filter(!is.na(agg_label)),
       aes(x = agg_label, fill = factor(defaut_securite, levels = c(1, 0), labels = c("Défaut d'équipement", "Équipé conforme")))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Défaut d'équipement" = "#FB8C00", "Équipé conforme" = "#78909C")) +
  labs(
    title = "Défaut d'équipement de sécurité (ceinture/casque) selon la zone",
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
    title = "Relation entre l'âge moyen des usagers et la sévérité",
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
# SECTION 4 : ANALYSE STATISTIQUE SPATIALE ET AUTOCORRÉLATION (INDICE DE MORAN)
# ==============================================================================

cat("\n=======================================================\n")
cat("      SECTION 4 : ANALYSE STATISTIQUE SPATIALE         \n")
cat("=======================================================\n")

# ------------------------------------------------------------------------------
# 4.1 Agrégation départementale métropolitaine et centroïdes
# ------------------------------------------------------------------------------
# Exclusion des DROM-COM pour respecter la contiguïté géographique terrestre
dep_spat <- accidents_modele %>%
  filter(!grepl("^9[78]", dep)) %>%
  group_by(dep, nom_dep) %>%
  summarise(
    n_accidents      = n(),
    severite_moyenne = round(mean(Nb_victimes), 3),
    lat              = mean(lat),
    long             = mean(long),
    .groups          = "drop"
  )

cat("\n=== Aperçu des données spatiales départementales (Top 6) ===\n")
print(head(dep_spat))

# ------------------------------------------------------------------------------
# 4.2 Matrice de poids spatiaux par inverse de la distance géodésique
# ------------------------------------------------------------------------------
dists <- as.matrix(dist(cbind(dep_spat$long, dep_spat$lat)))
dists_inv <- 1 / dists
diag(dists_inv) <- 0 # Diagonale nulle (un département n'est pas son propre voisin)

# ------------------------------------------------------------------------------
# 4.3 Test d'autocorrélation spatiale globale de Moran
# ------------------------------------------------------------------------------
moran_res <- Moran.I(dep_spat$severite_moyenne, dists_inv)

cat("\n=== RÉSULTATS DU TEST D'AUTOCORRÉLATION DE MORAN ===\n")
print(moran_res)

conclusion_moran <- ifelse(
  moran_res$p.value < 0.05,
  "Rejet de H0 : Autocorrélation spatiale positive très significative (la sévérité dépend de l'espace).",
  "Non-rejet de H0 : Pas de dépendance spatiale significative."
)
cat("Conclusion :", conclusion_moran, "\n")

# ------------------------------------------------------------------------------
# 4.4 Cartographie thématique de la sévérité départementale
# ------------------------------------------------------------------------------
fr_map <- map_data("france")

ggplot() +
  geom_polygon(data = fr_map, aes(x = long, y = lat, group = group),
               fill = "#F5F5F5", color = "#BDBDBD", linewidth = 0.3) +
  geom_point(data = dep_spat, 
             aes(x = long, y = lat, color = severite_moyenne, size = n_accidents),
             alpha = 0.85) +
  scale_color_gradient(low = "#42A5F5", high = "#E53935", 
                       name = "Sévérité\nmoyenne") +
  scale_size_continuous(range = c(2, 6), name = "Nb accidents") +
  coord_fixed(1.3) +
  labs(
    title = "Sévérité moyenne des accidents par département (France 2024)",
    subtitle = paste0("Indice de Moran I = ", round(moran_res$observed, 3), 
                      " (p-value = ", format.pval(moran_res$p.value, digits = 3), ")"),
    x = "", y = ""
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "gray30"),
    legend.position = "right"
  )




# ==============================================================================
# SECTION 5 : MODÉLISATION LINÉAIRE MULTIPLE ET SÉLECTION DE VARIABLES (PARTIES 3 & 4)
# ==============================================================================

cat("\n=======================================================\n")
cat("  SECTION 5 : MODÉLISATION LINÉAIRE ET SÉLECTION STEP   \n")
cat("=======================================================\n")

# 5.1 Estimation du modèle complet
mod_complet <- lm(
  Nb_victimes ~ saison + atm_label + agg_label + lum_label + col_regroup + 
    vma + nb_vehicules_impliquees + implique_pieton + implique_jeune_18_24 + 
    implique_senior_65plus + choc_obstacle_fixe + defaut_securite, 
  data = accidents_modele
)

cat("\n=== 5.1 Résumé du modèle linéaire multiple complet ===\n")
print(summary(mod_complet))
cat("AIC du modèle complet :", round(AIC(mod_complet), 2), "\n")

# 5.2 Procédure de sélection pas-à-pas de variables (stepAIC direction = 'both')
cat("\n=== 5.2 Procédure de sélection de variables pas-à-pas (stepAIC) ===\n")
mod_optimal <- MASS::stepAIC(mod_complet, direction = "both", trace = FALSE)

cat("\n=== Modèle optimal retenu par AIC ===\n")
print(summary(mod_optimal))
cat(sprintf("AIC initial : %.2f --> AIC optimal : %.2f\n", AIC(mod_complet), AIC(mod_optimal)))

# 5.3 Audit des résidus (Conditions de Gauss-Markov)
cat("\n=== 5.3 Audit des conditions de validité des résidus ===\n")
residus <- residuals(mod_optimal)
valeurs_ajustees <- fitted(mod_optimal)

cat("Distribution des résidus :\n")
print(summary(residus))

ggplot(data.frame(Fitted = valeurs_ajustees, Residuals = residus), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.08, color = "#546E7A") +
  geom_hline(yintercept = 0, color = "#D32F2F", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", color = "#1E88E5", se = FALSE) +
  labs(
    title = "Diagnostic des résidus du modèle linéaire optimal",
    x = "Valeurs prédites (ajustées)",
    y = "Résidus ordinaires"
  ) +
  theme_minimal(base_size = 12)

cat("\n=======================================================\n")
cat("          FIN DE L'EXÉCUTION DU SCRIPT R              \n")
cat("=======================================================\n")
