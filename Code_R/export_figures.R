# ==============================================================================
# EXPORT DES FIGURES HAUTE RÉSOLUTION POUR LE RAPPORT LATEX
# ==============================================================================

library(dplyr)
library(ggplot2)
library(maps)

accidents_modele <- read.csv("Ressources/accidents_modele.csv")
dir.create("Rapport/figures", showWarnings = FALSE, recursive = TRUE)

theme_rapport <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5),
    legend.position = "none"
  )

cat("--> Génération des figures pour le rapport LaTeX...\n")

# 1. Distribution de la variable cible Y (échelle logarithmique)
p1 <- ggplot(accidents_modele, aes(x = Nb_victimes)) +
  geom_bar(fill = "#1E88E5", color = "white", width = 0.8) +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50)) +
  labs(
    title = "Distribution du nombre de victimes par accident corporel",
    x = "Nombre de victimes (Y)",
    y = "Nombre d'accidents (échelle log10)"
  ) +
  theme_rapport
ggsave("Rapport/figures/g1_nb_victimes.png", p1, width = 8, height = 4.5, dpi = 300)

# 2. Sévérité par saison
stats_saison <- accidents_modele %>%
  group_by(saison) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p2 <- ggplot(stats_saison, aes(x = saison, y = Moyenne, fill = saison)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), vjust = -0.5, fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, 1.6)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Sévérité moyenne des accidents selon la saison",
    x = "Saison", y = "Nombre moyen de victimes par accident"
  ) +
  theme_rapport
ggsave("Rapport/figures/biv_saison.png", p2, width = 7, height = 4.2, dpi = 300)

# 3. Sévérité par météo
stats_atm <- accidents_modele %>%
  group_by(atm_label) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p3 <- ggplot(stats_atm, aes(x = reorder(atm_label, Moyenne), y = Moyenne, fill = atm_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Moyenne, 2)), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.8)) +
  labs(
    title = "Sévérité moyenne selon les conditions météorologiques",
    x = "Conditions atmosphériques", y = "Nombre moyen de victimes"
  ) +
  theme_rapport
ggsave("Rapport/figures/biv_atm.png", p3, width = 8, height = 4.8, dpi = 300)

# 4. Sévérité par agglomération
stats_agg <- accidents_modele %>%
  group_by(agg_label) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p4 <- ggplot(stats_agg, aes(x = agg_label, y = Moyenne, fill = agg_label)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Moyenne, " vict.")), vjust = -0.5, fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, 1.8)) +
  scale_fill_manual(values = c("#29B6F6", "#E53935")) +
  labs(
    title = "Sévérité moyenne : En agglomération vs Hors agglomération",
    x = "Localisation", y = "Nombre moyen de victimes"
  ) +
  theme_rapport
ggsave("Rapport/figures/biv_agg.png", p4, width = 6.5, height = 4.2, dpi = 300)

# 5. Sévérité par luminosité
stats_lum <- accidents_modele %>%
  group_by(lum_label) %>%
  summarise(Moyenne = round(mean(Nb_victimes), 3), .groups = "drop")
p5 <- ggplot(stats_lum, aes(x = reorder(lum_label, Moyenne), y = Moyenne, fill = lum_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Moyenne, 2)), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.8)) +
  labs(
    title = "Sévérité moyenne selon les conditions de luminosité",
    x = "Luminosité", y = "Nombre moyen de victimes"
  ) +
  theme_rapport
ggsave("Rapport/figures/biv_lum.png", p5, width = 8, height = 4.5, dpi = 300)

# 6. Sévérité par typologie de collision
biv_col <- accidents_modele %>%
  group_by(col_regroup) %>%
  summarise(Moyenne_victimes = round(mean(Nb_victimes), 3), .groups = "drop")
p6 <- ggplot(biv_col, aes(x = reorder(col_regroup, Moyenne_victimes), y = Moyenne_victimes, fill = col_regroup)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Moyenne_victimes, 2)), hjust = -0.2, fontface = "bold", size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2.5)) +
  labs(
    title = "Sévérité moyenne par typologie de collision",
    x = "Typologie de collision", y = "Nombre moyen de victimes"
  ) +
  theme_rapport
ggsave("Rapport/figures/biv_col.png", p6, width = 8.5, height = 4.8, dpi = 300)

# 7. Croisement Luminosité x Agglomération (V de Cramer fort)
p7 <- ggplot(accidents_modele, aes(x = agg_label, fill = lum_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2", name = "Luminosité") +
  labs(
    title = "Répartition de la luminosité selon la localisation",
    x = "Localisation", y = "Pourcentage d'accidents"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    legend.position = "right"
  )
ggsave("Rapport/figures/cramer_lum_agg.png", p7, width = 8, height = 4.8, dpi = 300)

# 8. Quanti x Quanti : VMA continue vs Sévérité
p8 <- ggplot(accidents_modele, aes(x = vma, y = Nb_victimes)) +
  geom_jitter(width = 2, height = 0.25, alpha = 0.08, color = "#1E88E5") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1) +
  scale_x_continuous(breaks = c(30, 50, 70, 80, 90, 110, 130)) +
  labs(
    title = "Relation entre la vitesse autorisée (VMA) et le nombre de victimes",
    x = "Vitesse Maximale Autorisée (km/h)",
    y = "Nombre de victimes par accident"
  ) +
  theme_rapport
ggsave("Rapport/figures/quanti_vma.png", p8, width = 8, height = 4.5, dpi = 300)

# 9. Quanti x Quanti : Âge moyen usagers vs Sévérité
p9 <- ggplot(accidents_modele, aes(x = age_moyen_usagers, y = Nb_victimes)) +
  geom_jitter(width = 0.5, height = 0.25, alpha = 0.08, color = "#43A047") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1) +
  labs(
    title = "Relation entre l'âge moyen des usagers et le nombre de victimes",
    x = "Âge moyen des usagers impliqués (années)",
    y = "Nombre de victimes par accident"
  ) +
  theme_rapport
ggsave("Rapport/figures/quanti_age.png", p9, width = 8, height = 4.5, dpi = 300)

# 10. Quanti x Quanti : Nombre de véhicules vs Sévérité
p10 <- ggplot(accidents_modele, aes(x = nb_vehicules_impliquees, y = Nb_victimes)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.08, color = "#8E24AA") +
  geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1) +
  scale_x_continuous(breaks = 1:11) +
  labs(
    title = "Relation entre le nombre de véhicules impliqués et le nombre de victimes",
    x = "Nombre de véhicules impliqués dans l'accident",
    y = "Nombre de victimes par accident"
  ) +
  theme_rapport
ggsave("Rapport/figures/quanti_vehicules.png", p10, width = 8, height = 4.5, dpi = 300)

# 11. Carte spatiale de Moran
dep_spat <- accidents_modele %>%
  filter(!grepl("^9[78]", dep)) %>%
  group_by(dep, nom_dep) %>%
  summarise(
    n_accidents = n(),
    severite_moyenne = round(mean(Nb_victimes), 3),
    lat = mean(lat),
    long = mean(long),
    .groups = "drop"
  )
fr_map <- map_data("france")
p11 <- ggplot() +
  geom_polygon(data = fr_map, aes(x = long, y = lat, group = group),
               fill = "#F5F5F5", color = "#BDBDBD", linewidth = 0.3) +
  geom_point(data = dep_spat,
             aes(x = long, y = lat, color = severite_moyenne, size = n_accidents),
             alpha = 0.85) +
  scale_color_gradient(low = "#42A5F5", high = "#E53935", name = "Sévérité\nmoyenne") +
  scale_size_continuous(range = c(2, 6), name = "Nb accidents") +
  coord_fixed(1.3) +
  labs(
    title = "Sévérité moyenne des accidents par département (France métropolitaine 2024)",
    x = "", y = ""
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    legend.position = "right"
  )
ggsave("Rapport/figures/carte_moran.png", p11, width = 8, height = 6, dpi = 300)

# 12. Diagnostic des résidus Gauss-Markov
mod_complet <- lm(
  Nb_victimes ~ saison + atm_label + agg_label + lum_label + col_regroup + 
    vma + nb_vehicules_impliquees + implique_pieton + implique_jeune_18_24 + 
    implique_senior_65plus + choc_obstacle_fixe + defaut_securite, 
  data = accidents_modele
)
mod_optimal <- MASS::stepAIC(mod_complet, direction = "both", trace = FALSE)
residus <- residuals(mod_optimal)
valeurs_ajustees <- fitted(mod_optimal)

p12 <- ggplot(data.frame(Fitted = valeurs_ajustees, Residuals = residus), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.08, color = "#546E7A") +
  geom_hline(yintercept = 0, color = "#D32F2F", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", color = "#1E88E5", se = FALSE) +
  labs(
    title = "Diagnostic des résidus du modèle linéaire optimal",
    x = "Valeurs prédites (ajustées)",
    y = "Résidus ordinaires"
  ) +
  theme_rapport
ggsave("Rapport/figures/diagnostic_residus.png", p12, width = 8, height = 4.8, dpi = 300)

# Copier également à la racine de Rapport pour flexibilité d inclusion LaTeX
file.copy(list.files("Rapport/figures", full.names = TRUE), "Rapport/", overwrite = TRUE)

cat("--> Toutes les figures ont été générées et exportées avec succès dans Rapport/figures/ et Rapport/ !\n")
