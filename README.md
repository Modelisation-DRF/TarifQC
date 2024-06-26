[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/Modelisation-DRF/RNatura2014/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modelisation-DRF/RNatura2014/actions/workflows/R-CMD-check.yaml)
## Le package TarifQC

Un package pour estimer la hauteur et le volume des arbres

Auteurs: Isabelle Auger - Ministère des Ressources Naturelles et des Forêts du Québec

Courriel: isabelle.auger@mrnf.gouv.qc.ca

## Introduction
Le package permet d'estimer la hauteur totale en mètre et le volume marchand brut en dm3 de chacun des arbres, regroupés en placette. L'estimation peut être déterministe ou stochastique.

## Documentation et références
Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.

Fortin, M., J. DeBlois, S. Bernier et G. Blais, 2007. Mise au point d’un tarif de cubage général pour les forêts québécoises : une approche pour mieux évaluer l’incertitude associée aux prévisions. For. Chron. 83: 754-765.

## Dépendences
Aucune dépendence à des packages externes à CRAN

## Comment obtenir le code source
Taper cette ligne dans une invite de commande pour cloner le dépôt dans un sous-dossier "tarifqc":

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
git clone https://github.com/Modelisation-DRF/TarifQC tarifqc
```

## Comment installer le package TarifQC dans R

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
library(remotes)
remotes::install_github("Modelisation-DRF/TarifQC")
```
## Exemple

Ce package inclut des objets de type data.frame contenent des listes d'arbres regroupées en placettes. Ces objets peuvent être utilisés pour essayer le package.

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
library(TarifQC)
DataHt <- relation_h_d(fic_arbres=fic_arbres_test)
DataHtVol <- cubage(fic_arbres=DataHt)
```
De l'aide supplémentaire peut être obtenu sur les fonctions
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
?relation_h_d
?cubage
```
Pour obtenir la liste des data.frame disponibles
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
data(package='TarifQC')
```
Pour une description du data.frame
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
?fic_arbres_test
```

## Historique des versions

| Date |  Version  | Issues |      Détails     |
|:-----|:---------:|:-------|:-----------------|
| 2024-05-23 | 1.1.6 |  | corriger fichier association du milieu pour le PEG, il manquait le milieu 9 |
| 2024-05-13 | 1.1.5 |  | corriger erreur fct f qui génère matrice covariances |
| 2024-04-15 | 1.1.4 |  | corriger bug quand nb_step>9 dans ht et vol |
| 2024-03-26 | 1.1.3 |  | déplacer les packages de depends à imports dans DESCRIPTION, utiliser la fct mvrnorm de rockchalk au lieu de MASS |
| 2024-02-22 | 1.1.2 |  | ajout de l'option na.rm=T dans le calcul de la st et densité de chaque placette |
| 2024-02-20 | 1.1.1 |  | correction de bugs mineurs détectés en utilisant un fichier de samare avec peu d'essences |
| 2024-02-08 | 1.1.0 | issue #1  | amélioration de la vitesse d'exécution en mode stochastique |
| 2023-11-30 | 1.0.0 | | première version stable |

