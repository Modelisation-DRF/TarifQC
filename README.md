## TarifQC

Un package pour estimer la hauteur et le volume des arbres

Auteurs: Isabelle Auger - Ministère des Ressources Naturelles et des Forêts du Québec

Courriel: isabelle.auger@mrnf.gouv.qc.ca

This R package is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This library is distributed with the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

See the license LGPL-3.0 at http://www.gnu.org/copyleft/lesser.html.

## Introduction
Le package permet d'estimer la hauteur totale en mètre et le volume marchand brut en dm3 de chacun des arbres, regroupés en placette. L'estimation peut être déterministe ou stochastique.

## Documentation et références
Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.

Fortin, M., J. DeBlois, S. Bernier et G. Blais, 2007. Mise au point d’un tarif de cubage général pour les forêts québécoises : une approche pour mieux évaluer l’incertitude associée aux prévisions. For. Chron. 83: 754-765.

## Dépendences
Ce package dépends des packages ExtractMap et TarifQC.

- TarifQC est disponible ici https://github.com/Modelisation-DRF/TarifQC

- ExtractMap est disponible sur demande.

## Comment obtenir le code source
Taper cette ligne dans une invite de commande pour cloner le dépôt dans un sous-dossier "natura3":

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
git clone https://github.com/Modelisation-DRF/Natura3 natura3
```

## Comment installer le package Natura3 dans R

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
require(remotes)
install_github("https://github.com/Modelisation-DRF/Natura3", ref="main", auth_token = "demander_un_token")
```
## Exemple

Ce package inclut des objets de type data.frame contenent des listes d'arbres regroupées en placettes. Ces objets peuvent être utilisés pour essayer le package.

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
library(Natura3)
data_simul <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5)
```
De l'aide supplémentaire peut être obtenu sur la fonction avec la commande
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
?SimulNatura
```
Pour obtenir la liste des data.frame disponibles
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
data(package='Natura3')
```
Pour une description du data.frame
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
?fichier_arbres_sanscov
```

## Historique des versions

| Date |  Version  | Features et bugs |      Détails     |
|:-----|:---------:|:-----------------|:-----------------|
| 2024-03-25 | 1.0.0 |  | Première version stable |
