# Étude Statistique et Prédictive des Accidents Corporels (France - 2024)

Ce projet universitaire (BUT Science des Données - 2ème année) propose une analyse approfondie des accidents corporels de la circulation survenus en France en 2024 à partir des bases officielles **BAAC** (Bulletin d'Analyse des Accidents Corporels) et des données démographiques légales de l'**INSEE**.

---

## 🎯 Problématique

> **« Dans quelle mesure la saisonnalité et les conditions météorologiques influencent-elles le nombre de victimes par accident, et observe-t-on une dépendance spatiale de cette sévérité à l'échelle départementale ? »**

L'étude s'articule autour de deux volets complémentaires :
1. **Volet Statistique Inférentielle & Spatiale (R)** : Analyse exploratoire, tests d'hypothèses non paramétriques (Kruskal-Wallis), autocorrélation spatiale (Indice de Moran) et modélisation GLM.
2. **Volet Machine Learning & Data Mining (Python)** : Évaluation de modèles prédictifs (k-NN, Random Forest, XGBoost) pour classifier et anticiper la sévérité d'un accident.

---

## 📁 Architecture du Projet

```text
Etude-Statistique-Accidents/
├── preparation_donnees.R    # Pipeline ETL : nettoyage, imputations et enrichissement
├── code_modelisation.R      # Analyse statistique, tests d'hypothèses et modèles sous R
├── Data_mining.ipynb        # Volet Machine Learning / Data Mining sous Python
│
├── Ressources/              # Données brutes et consolidées
│   ├── caract-2024.csv      # Caractéristiques générales des accidents (météo, luminosité, localisation)
│   ├── lieux-2024.csv       # Infrastructure routière et VMA (Vitesse Maximale Autorisée)
│   ├── vehicules-2024.csv   # Véhicules impliqués et typologies
│   ├── usagers-2024.csv     # Usagers, gravité des blessures et équipements de sécurité
│   ├── Insee.csv            # Population légale 2024 par département (normalisation de l'exposition)
│   └── accidents_2024.csv   # Table maîtresse enrichie et prête pour la modélisation
│
├── UML/                     # Rétroconception relationnelle et modèle décisionnel
├── Rplots/                  # Graphiques et visualisations générés (ignoré dans Git)
├── .gitignore               # Exclusion des fichiers temporaires et graphiques locaux
└── README.md                # Documentation générale du projet
```

---

## ⚙️ Méthodologie et Préparation des Données (`preparation_donnees.R`)

Le script de préparation assure la reproductibilité et la qualité des données en amont de toute modélisation :
- **Variable cible (`Nb_victimes`)** : Calculée à partir de la table `usagers` en comptabilisant uniquement les personnes non indemnes (`grav != 1`).
- **Filtrage de cohérence** : Suppression des accidents sans type de collision valide (`col == -1`).
- **Imputation de la VMA** :
  - Détection et correction des erreurs de frappe (ex. 500 ou 900 ramenés à 50 et 90 km/h).
  - Traitement des valeurs non renseignées (`-1`) et aberrantes selon le Code de la route : imputation à 50 km/h en agglomération (`agg == 2`) et à 80 km/h hors agglomération (`agg == 1`).
- **Enrichissement de la table accidents (Cas A)** :
  - Nombre de véhicules impliqués (`nb_vehicules_impliquees`).
  - Indicatrices de présence par typologie de véhicule (`implique_voiture`, `implique_moto`, `implique_velo_edp`, `implique_poids_lourd`, `implique_transp_commun`).
  - Présence de piétons (`implique_pieton`) et défaut d'équipement de sécurité (`defaut_securite`).
- **Correction du biais démographique** : Intégration des populations municipales INSEE pour calculer des taux d'accidentalité standardisés pour 100 000 habitants à l'échelle départementale.

---

## 🚀 Ordre d'Exécution (Pipeline)

### 1. Préparation des données (R)
Exécuter le script de nettoyage et d'enrichissement. Celui-ci génère automatiquement le fichier consolidé `Ressources/accidents_2024.csv` :
```r
source("preparation_donnees.R")
```

### 2. Analyses statistiques et graphiques (R)
Lancer le script principal pour reproduire les visualisations, les tests de Kruskal-Wallis et les analyses spatiales :
```r
source("code_modelisation.R")
```

### 3. Modélisation prédictive (Python)
Ouvrir le notebook Jupyter pour évaluer les modèles de Machine Learning sur le dataset préparé :
```bash
jupyter notebook Data_mining.ipynb
```

---

## 🛠️ Technologies Utilisées
- **Langages** : R, Python (Jupyter)
- **Librairies R** : `tidyverse` (`dplyr`, `ggplot2`, `tidyr`, `readr`), `lubridate`, `spdep`, `sf`
- **Librairies Python** : `pandas`, `numpy`, `scikit-learn`, `matplotlib`, `seaborn`
- **Données** : Ministère de l'Intérieur (ONISR / data.gouv.fr) & INSEE
