% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fic_arbres_test}
\alias{fic_arbres_test}
\title{Table avec la liste d'arbres de deux placettes du réseau des placettes temporaire du Québec}
\format{
## `fic_arbres_test`
A data frame with 79 lignes et 13 colonnes:
\describe{
  \item{id_pe}{Identifiant de la placette}
  \item{sdom_bio}{Code du sous-domaine bioclimatique}
  \item{type_eco}{Code du type écologique}
  \item{veg_pot}{Code de la végétation potentielle, 3 premiers caractères du type écologique}
  \item{milieu}{Code du milieu physique, , 4e caractère du type écologique}
  \item{altitude}{Altitude (m)}
  \item{t_ma}{Température annuelle moyenne}
  \item{tige_ha}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
  \item{nb_tige}{tiges_ha converti pour une placette de 400 m2, donc nombre de tiges dans 400m2}
  \item{no_arbre}{Identifiant de la combinaison dhpcm/essence}
  \item{essence}{Code d'essence}
  \item{dhpcm}{Classe de DHP (cm)}
}
}
\usage{
fic_arbres_test
}
\description{
Deux placettes des PET avec leur liste d'arbres
}
\keyword{datasets}
