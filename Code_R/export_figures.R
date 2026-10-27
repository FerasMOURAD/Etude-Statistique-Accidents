# ==============================================================================
# EXPORT DES FIGURES HAUTE RÉSOLUTION POUR LE RAPPORT LATEX
# Optimisé pour une lisibilité parfaite (polices agrandies, contraste élevé,
# suppression des titres internes redondants avec les captions LaTeX).
# ==============================================================================

library(dplyr)
library(ggplot2)

accidents_modele <- read.csv("Ressources/accidents_modele.csv")
dir.create("Rapport/figures", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# THÈMES GRAPHIQUES STANDARDISÉS
# ------------------------------------------------------------------------------

# Thème pour les figures en demi-page (minipage LaTeX 0.48\linewidth)
# Police de base 16pt pour qu'après réduction d'échelle par LaTeX, le texte mesure ~10.5pt
theme_demi <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", size = 15, color = "gray10"),
    axis.text = element_text(size = 13, face = "bold", color = "gray20"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(t = 12, r = 15, b = 10, l = 10),
    legend.position = "none"
  )

# Thème pour les figures en pleine largeur (0.70\linewidth à 0.85\linewidth dans LaTeX)
theme_pleine <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", size = 14, color = "gray10"),
    axis.text = element_text(size = 12, face = "bold", color = "gray20"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    legend.position = "none"
  )

cat("--> Génération des figures haute lisibilité pour le rapport LaTeX...\n")

# ==============================================================================
# SECTION 2 : FIGURES UNIVARIÉES
# ==============================================================================

# 1. Distribution de la variable cible Y (échelle logarithmique horizontale)
p1 <- accidents_modele %>%
  count(Nb_victimes) %>%
  ggplot() +
  geom_rect(aes(ymin = Nb_victimes - 0.45, ymax = Nb_victimes + 0.45, xmin = 0.5, xmax = n),
            fill = "#1E88E5", color = "white", linewidth = 0.3) +
  scale_x_log10(limits = c(0.5, 60000), breaks = c(1, 10, 100, 1000, 10000),
                labels = c("1", "10", "100", "1 000", "10 000")) +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20, 30, 40, 50)) +
  labs(
    x = "Nombre d'accidents (échelle logarithmique log10)",
    y = "Nombre de victimes par accident (Y)"
  ) +
  theme_pleine
ggsave("Rapport/figures/g1_nb_victimes.png", p1, width = 7.5, height = 4.2, dpi = 300)

# 1b. Distribution par saison (univarié, demi-page)
ordre_saisons <- c("Hiver", "Printemps", "Ete", "Automne")
lab_saisons <- c("Hiver", "Printemps", "Été", "Automne")
p_saison <- accidents_modele %>%
  mutate(saison = factor(saison, levels = ordre_saisons, labels = lab_saisons)) %>%
  count(saison) %>%
  ggplot(aes(x = saison, y = n, fill = saison)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = format(n, big.mark = " ")),
            vjust = -0.3, size = 5.0, fontface = "bold", color = "gray10") +
  scale_y_continuous(limits = c(0, 15500), labels = scales::label_number(big.mark = " ")) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Saison", y = "Nombre d'accidents") +
  theme_demi
ggsave("Rapport/figures/univ_saison.png", p_saison, width = 5.2, height = 3.9, dpi = 300)

# 1c. Distribution par mois (univarié, demi-page - sans chiffres pour éviter tout chevauchement)
p_mois <- accidents_modele %>%
  filter(!is.na(mois)) %>%
  count(mois) %>%
  mutate(mois_lab = factor(mois, levels = 1:12, 
                           labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", 
                                      "Juil", "Août", "Sept", "Oct", "Nov", "Déc"))) %>%
  ggplot(aes(x = mois_lab, y = n)) +
  geom_col(fill = "#43A047", width = 0.72) +
  scale_y_continuous(limits = c(0, 5500), labels = scales::label_number(big.mark = " ")) +
  labs(x = "Mois", y = "Nombre d'accidents") +
  theme_demi +
  theme(axis.text.x = element_text(size = 11.5, angle = 30, hjust = 1))
ggsave("Rapport/figures/univ_mois.png", p_mois, width = 5.2, height = 3.9, dpi = 300)

# 1d. Distribution par jour de la semaine (univarié, demi-page)
ordre_jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
p_jour <- accidents_modele %>%
  filter(!is.na(jour_semaine)) %>%
  count(jour_semaine) %>%
  mutate(jour_semaine = factor(jour_semaine, levels = ordre_jours)) %>%
  ggplot(aes(x = jour_semaine, y = n)) +
  geom_col(fill = "#5C6BC0", width = 0.65) +
  geom_text(aes(label = format(n, big.mark = " ")),
            vjust = -0.3, size = 4.4, fontface = "bold", color = "gray10") +
  scale_y_continuous(limits = c(0, 9500), labels = scales::label_number(big.mark = " ")) +
  labs(x = "Jour de la semaine", y = "Nombre d'accidents") +
  theme_demi +
  theme(axis.text.x = element_text(size = 11.5, angle = 25, hjust = 1))
ggsave("Rapport/figures/univ_jour.png", p_jour, width = 5.2, height = 3.9, dpi = 300)

# 1e. Distribution horaire (rythme circadien univarié, demi-page)
p_heure <- ggplot(accidents_modele %>% filter(!is.na(heure)), aes(x = heure)) +
  geom_bar(fill = "#FF7043", color = "white", width = 0.85) +
  scale_x_continuous(breaks = seq(0, 23, by = 4)) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(x = "Heure de la journée (0h - 23h)", y = "Nombre d'accidents") +
  theme_demi
ggsave("Rapport/figures/univ_heure.png", p_heure, width = 5.2, height = 3.9, dpi = 300)

# 1f. Distribution selon la luminosité (univarié, pleine largeur)
p_lum <- accidents_modele %>%
  filter(!is.na(lum_label)) %>%
  count(lum_label) %>%
  ggplot(aes(x = reorder(lum_label, n), y = n, fill = lum_label)) +
  geom_col(width = 0.58, show.legend = FALSE) +
  geom_text(aes(label = format(n, big.mark = " ")),
            hjust = -0.08, size = 4.6, fontface = "bold", color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 39000), labels = scales::label_number(big.mark = " ")) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  labs(x = "", y = "Nombre d'accidents") +
  theme_pleine
ggsave("Rapport/figures/univ_lum.png", p_lum, width = 7.5, height = 3.6, dpi = 300)

# 1g. Distribution selon les conditions atmosphériques (univarié, pleine largeur)
p_atm <- accidents_modele %>%
  filter(!is.na(atm_label)) %>%
  count(atm_label) %>%
  ggplot(aes(x = reorder(atm_label, n), y = n, fill = atm_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = format(n, big.mark = " ")),
            hjust = -0.08, size = 4.4, fontface = "bold", color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 45000), labels = scales::label_number(big.mark = " ")) +
  labs(x = "", y = "Nombre d'accidents") +
  theme_pleine
ggsave("Rapport/figures/univ_atm.png", p_atm, width = 7.5, height = 4.5, dpi = 300)

# 1h. Distribution agglomération vs hors agglomération (univarié)
p_agg <- accidents_modele %>%
  filter(!is.na(agg_label)) %>%
  count(agg_label) %>%
  ggplot(aes(x = agg_label, y = n, fill = agg_label)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = format(n, big.mark = " ")),
            vjust = -0.3, size = 5.0, fontface = "bold", color = "gray10") +
  scale_y_continuous(limits = c(0, 36000), labels = scales::label_number(big.mark = " ")) +
  scale_fill_manual(values = c("#29B6F6", "#E53935")) +
  labs(x = "Zone de localisation", y = "Nombre d'accidents") +
  theme_pleine
ggsave("Rapport/figures/univ_agg.png", p_agg, width = 6.0, height = 3.8, dpi = 300)

# 1i. Distribution par typologie de collision (univarié, pleine largeur)
p_col <- accidents_modele %>%
  filter(!is.na(col_regroup)) %>%
  count(col_regroup) %>%
  ggplot(aes(x = reorder(col_regroup, n), y = n, fill = col_regroup)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = format(n, big.mark = " ")),
            hjust = -0.08, size = 4.4, fontface = "bold", color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 26000), labels = scales::label_number(big.mark = " ")) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "", y = "Nombre d'accidents") +
  theme_pleine
ggsave("Rapport/figures/univ_col.png", p_col, width = 7.5, height = 4.0, dpi = 300)

# 1j. Distribution par type d'intersection (univarié, pleine largeur)
lab_int <- c("1" = "Hors intersection", "2" = "En X", "3" = "En T",
             "4" = "En Y", "5" = "> 4 branches", "6" = "Rond-point",
             "7" = "Place", "8" = "Passage à niveau", "9" = "Autre")
p_int <- accidents_modele %>%
  filter(!is.na(int)) %>%
  mutate(int_label = lab_int[as.character(int)]) %>%
  count(int_label) %>%
  ggplot(aes(x = reorder(int_label, n), y = n)) +
  geom_col(fill = "#26A69A", width = 0.62) +
  geom_text(aes(label = format(n, big.mark = " ")),
            hjust = -0.08, size = 4.4, fontface = "bold", color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 37000), labels = scales::label_number(big.mark = " ")) +
  labs(x = "", y = "Nombre d'accidents") +
  theme_pleine
ggsave("Rapport/figures/univ_int.png", p_int, width = 7.5, height = 4.2, dpi = 300)

# 1k. Top 20 départements pour 100 000 habitants (univarié géographique)
top_dep <- accidents_modele %>%
  filter(!is.na(population) & !is.na(dep)) %>%
  count(dep, nom_dep, population, name = "nb_accidents") %>%
  mutate(ratio = round((nb_accidents / population) * 100000, 2)) %>%
  arrange(desc(ratio)) %>%
  slice_head(n = 20)

p_top_dep <- ggplot(top_dep, aes(x = reorder(nom_dep, ratio), y = ratio)) +
  geom_col(fill = "#8E24AA", width = 0.68) +
  geom_text(aes(label = round(ratio, 1)), hjust = -0.15, size = 4.2, fontface = "bold", color = "gray15") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 225)) +
  labs(x = "", y = "Accidents pour 100 000 habitants") +
  theme_pleine
ggsave("Rapport/figures/univ_top_dep.png", p_top_dep, width = 7.5, height = 5.4, dpi = 300)

# ==============================================================================
# SECTION 3 : FIGURES BIVARIÉES ET ANALYSES COMPARATIVES
# ==============================================================================

# 2. Sévérité par saison
stats_saison <- accidents_modele %>%
  mutate(saison = factor(saison, levels = ordre_saisons, labels = lab_saisons)) %>%
  group_by(saison) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p2 <- ggplot(stats_saison, aes(x = saison, y = Moyenne, fill = saison)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), vjust = -0.3, fontface = "bold", size = 4.8, color = "gray10") +
  scale_y_continuous(limits = c(0, 1.65)) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Saison", y = "Nombre moyen de victimes") +
  theme_pleine
ggsave("Rapport/figures/biv_saison.png", p2, width = 6.5, height = 3.8, dpi = 300)

# 3. Sévérité par météo
stats_atm <- accidents_modele %>%
  group_by(atm_label) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p3 <- ggplot(stats_atm, aes(x = reorder(atm_label, Moyenne), y = Moyenne, fill = atm_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Moyenne, 2), " vict.")), hjust = -0.12, size = 4.2, fontface = "bold", color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.85)) +
  labs(x = "", y = "Nombre moyen de victimes") +
  theme_pleine
ggsave("Rapport/figures/biv_atm.png", p3, width = 7.5, height = 4.5, dpi = 300)

# 4. Sévérité par agglomération
stats_agg <- accidents_modele %>%
  group_by(agg_label) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p4 <- ggplot(stats_agg, aes(x = agg_label, y = Moyenne, fill = agg_label)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), vjust = -0.3, fontface = "bold", size = 4.8, color = "gray10") +
  scale_y_continuous(limits = c(0, 1.85)) +
  scale_fill_manual(values = c("#29B6F6", "#E53935")) +
  labs(x = "Localisation", y = "Nombre moyen de victimes") +
  theme_pleine
ggsave("Rapport/figures/biv_agg.png", p4, width = 6.0, height = 3.8, dpi = 300)

# 5. Sévérité par luminosité
stats_lum <- accidents_modele %>%
  group_by(lum_label) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p5 <- ggplot(stats_lum, aes(x = reorder(lum_label, Moyenne), y = Moyenne, fill = lum_label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Moyenne, 2), " vict.")), hjust = -0.12, size = 4.2, fontface = "bold", color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.85)) +
  labs(x = "", y = "Nombre moyen de victimes") +
  theme_pleine
ggsave("Rapport/figures/biv_lum.png", p5, width = 7.5, height = 3.8, dpi = 300)

# 6. Sévérité par typologie de collision
biv_col <- accidents_modele %>%
  group_by(col_regroup) %>%
  summarise(Moyenne_victimes = round(mean(Nb_victimes), 3), .groups = "drop")
p6 <- ggplot(biv_col, aes(x = reorder(col_regroup, Moyenne_victimes), y = Moyenne_victimes, fill = col_regroup)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Moyenne_victimes, 2), " vict.")), hjust = -0.12, fontface = "bold", size = 4.2, color = "gray10") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2.55)) +
  labs(x = "", y = "Nombre moyen de victimes") +
  theme_pleine
ggsave("Rapport/figures/biv_col.png", p6, width = 7.5, height = 4.0, dpi = 300)

# 7. Croisement Luminosité x Agglomération (V de Cramer)
p7 <- ggplot(accidents_modele, aes(x = agg_label, fill = lum_label)) +
  geom_bar(position = "fill", width = 0.55) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2", name = "Luminosité") +
  labs(x = "Localisation", y = "Pourcentage d'accidents") +
  theme_pleine +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )
ggsave("Rapport/figures/cramer_lum_agg.png", p7, width = 7.5, height = 4.2, dpi = 300)

# 8. Quanti x Quanti : VMA continue vs Sévérité
p8 <- ggplot(accidents_modele, aes(x = vma, y = Nb_victimes)) +
  geom_jitter(width = 2, height = 0.25, alpha = 0.08, color = "#1E88E5") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1.2) +
  scale_x_continuous(breaks = c(30, 50, 70, 80, 90, 110, 130)) +
  labs(x = "Vitesse Maximale Autorisée (km/h)", y = "Nombre de victimes (Y)") +
  theme_pleine
ggsave("Rapport/figures/quanti_vma.png", p8, width = 7.5, height = 4.2, dpi = 300)

# 9. Quanti x Quanti : Âge moyen usagers vs Sévérité
p9 <- ggplot(accidents_modele, aes(x = age_moyen_usagers, y = Nb_victimes)) +
  geom_jitter(width = 0.5, height = 0.25, alpha = 0.08, color = "#43A047") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1.2) +
  labs(x = "Âge moyen des usagers impliqués (années)", y = "Nombre de victimes (Y)") +
  theme_pleine
ggsave("Rapport/figures/quanti_age.png", p9, width = 7.5, height = 4.2, dpi = 300)

# 10. Quanti x Quanti : Nombre de véhicules vs Sévérité
p10 <- ggplot(accidents_modele, aes(x = nb_vehicules_impliquees, y = Nb_victimes)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.08, color = "#8E24AA") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1.2) +
  scale_x_continuous(breaks = 1:11) +
  labs(x = "Nombre de véhicules impliqués dans l'accident", y = "Nombre de victimes (Y)") +
  theme_pleine
ggsave("Rapport/figures/quanti_vehicules.png", p10, width = 7.5, height = 4.2, dpi = 300)

# ==============================================================================
# SECTIONS 4 ET 5 : les figures de la régression (diag_modele.png, boxcox.png)
# et des cartes (carte_victimes.png, carte_residus.png) sont générées à la fin
# de code_modelisation.R, car elles utilisent le modèle final et les résidus
# calculés dans ce script.
# ==============================================================================

cat("--> Toutes les figures ont été générées et exportées avec succès dans Rapport/figures/ !\n")
