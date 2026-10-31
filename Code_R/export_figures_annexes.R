# Export des figures non incluses dans le rapport
library(ggplot2)
library(dplyr)
library(sf)
library(spdep)

out_dir <- "Rapport/figures_annexes"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

accidents_modele <- read.csv("Ressources/accidents_modele.csv")

# 1. Saison x Météo
p1 <- ggplot(accidents_modele %>% filter(!is.na(saison), !is.na(atm_label)),
       aes(x = saison, fill = atm_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Répartition des conditions atmosphériques selon la saison",
    x = "Saison", y = "Proportion", fill = "Météo"
  ) +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "1_saison_meteo.png"), p1, width = 7, height = 4.5, dpi = 200)

# 2. Météo x Zone (Agglo)
p2 <- ggplot(accidents_modele %>% filter(!is.na(atm_label), !is.na(agg_label)),
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
ggsave(file.path(out_dir, "2_meteo_agglo.png"), p2, width = 7, height = 4.5, dpi = 200)

# 3. Piétons x Zone (Agglo)
p3 <- ggplot(accidents_modele %>% filter(!is.na(agg_label)),
       aes(x = agg_label, fill = factor(implique_pieton, levels = c(1, 0), labels = c("Avec piéton", "Sans piéton")))) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Avec piéton" = "#E53935", "Sans piéton" = "#B0BEC5")) +
  labs(
    title = "Présence de piétons selon la localisation (agglomération vs hors agglomération)",
    x = "", y = "Proportion", fill = "Statut piéton"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "3_pieton_agglo.png"), p3, width = 6.5, height = 4.5, dpi = 200)

# 4. Poids lourd x Collision
p4 <- ggplot(accidents_modele %>% filter(!is.na(col_regroup)),
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
ggsave(file.path(out_dir, "4_poids_lourd_collision.png"), p4, width = 7, height = 4.5, dpi = 200)

# 5. Non-port ceinture/casque x Zone
p5 <- ggplot(accidents_modele %>% filter(!is.na(agg_label)),
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
ggsave(file.path(out_dir, "5_non_port_zone.png"), p5, width = 6.5, height = 4.5, dpi = 200)

# 6. Boxplot VMA selon Zone
p6 <- ggplot(accidents_modele %>% filter(!is.na(agg_label), !is.na(vma)),
       aes(x = agg_label, y = vma, fill = agg_label)) +
  geom_boxplot(width = 0.4, outlier.alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = c("Hors agglomération" = "#5C6BC0", "En agglomération" = "#26A69A")) +
  labs(
    title = "Distribution de la vitesse maximale autorisée (VMA) par zone",
    subtitle = "Le losange blanc représente la moyenne",
    x = "", y = "VMA (km/h)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggsave(file.path(out_dir, "6_boxplot_vma_zone.png"), p6, width = 6.5, height = 4.5, dpi = 200)

# 7. Histogramme des résidus
train <- subset(accidents_modele, echantillon == "train")
mod <- lm(Nb_victimes ~ atm_label + agg_label + col_regroup + nb_vehicules_impliquees + 
    nb_usagers + implique_pieton + implique_jeune_18_24 + choc_obstacle_fixe + 
    non_port_ceinture_casque + implique_voiture + implique_moto + 
    implique_velo_edp + implique_poids_lourd + implique_transp_commun, data = train)
res_std <- rstandard(mod)

png(file.path(out_dir, "7_hist_residus.png"), width = 1400, height = 900, res = 200)
hist(res_std, breaks = 50, main = "Histogramme des résidus standardisés",
     xlab = "Résidus standardisés", ylab = "Fréquence", col = "skyblue", border = "white")
dev.off()

# 8. Réseau de contiguïté des départements (queen)
fr_dep <- st_read("Ressources/departements-version-simplifiee.geojson", quiet = TRUE)
dep_counts <- accidents_modele %>% count(dep)
fr_dep <- merge(fr_dep, dep_counts, by.x = "code", by.y = "dep")
voisins_contig <- poly2nb(fr_dep, queen = TRUE)

png(file.path(out_dir, "8_reseau_voisinage_queen.png"), width = 1400, height = 1200, res = 200)
plot(st_geometry(fr_dep), border = "gray70", col = "#f9f9f9", main = "Réseau de contiguïté spatiale des 94 départements (voisinage Queen)")
plot(voisins_contig, st_geometry(fr_dep), add = TRUE, col = "#d32f2f", lwd = 1.5, pch = 19, cex = 0.6)
dev.off()

# Copie des anciennes figures archivées
if (file.exists("Rapport/figures/carte_moran.png")) {
  file.copy("Rapport/figures/carte_moran.png", file.path(out_dir, "9_ancienne_carte_moran.png"), overwrite = TRUE)
}
if (file.exists("Rapport/figures/diagnostic_residus.png")) {
  file.copy("Rapport/figures/diagnostic_residus.png", file.path(out_dir, "10_ancien_diag_anglais.png"), overwrite = TRUE)
}
cat("Génération terminée !\n")
