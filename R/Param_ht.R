################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#                                                              #
#   Utilise                                                    #
#   ht_param_fixe.rda                                          #
#   ht_param_cov.rda                                           #
#   ht_param_random.rda                                        #
#   ht_liste_ess.rda                                           #
#                                                              #
################################################################


#' Génère les paramètres pour estimer la hauteur totale d'un arbre avec l'équation de Auger (2016)
#'
#' @description Génère les paramètres pour estimer la hauteur totale d'un arbre avec l'équation de Auger (2016).
#' Les paramètres peuvent être générés de façon déterministe ou stochastique.
#' Si le mode stochastique est utilisé, les paramètres seront générés pour tous les arbres/itétations/années.
#'
#' @details
#' L'équation pour estimer la hauteur totale a été étalonnée avec un modèle linéaire mixte, par essence (Auger 2016).
#' Le modèle inclut un effet aléatoire de placette et une corrélation de type CorCar1 sur les erreurs résiduelles.
#'
#' Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et
#' du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère
#' des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.
#'
#' @param fic_arbres Une table contenant la liste d'arbres dont la hauteur est à estimer. La table doit contenir les variables \code{no_arbre} (l'identifiant de l'arbre) et \code{id_pe} (l'identifiant de la placette).
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer la hauteur pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param dt La durée de l'intervalle de temps entre deux mesures d'un même arbre si \code{nb_step>1} (par défaut 10), ignoré si \code{mode_simul="DET"}.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return La fonction retourne une liste de listes, soit une liste par essence pour laquelle il y a une équation de hauteur.
#' Pour une essence, la liste contient 4 éléments:
#' \enumerate{
#'   \item essence: le code de l'essence
#'   \item effet_fixe: une table avec une colonne pour chacun des paramètres des effets fixes de l'équation et une ligne par iteration. Une seule ligne si \code{mode_simul="DET"}.
#'   \item random_placette: une table avec 4 colonnes: effet aléatoire de placette, le numéro de l'itération, l'identifiant de la placette et le code de l'essence. Une ligne par placette/itération. 0 si \code{mode_simul="DET"}.
#'   \item erreur_residuelle: une table avec 6 colonnes:  erreur résiduelle, identifiant de la placette, identifiant de l'arbre, numéro de step, numéro de l'itération et le code de l'essence. Une ligne par arbre/placette/itération/step. 0 si \code{mode_simul="DET"}.
#' }
#' @export
#'
#' @examples
#' # Mode déterministe
#' parametre_ht_dhp <- param_ht(fic_arbres=fic_arbres_test)
#'
#' # Mode stochastique, pour une seule année et 10 itérations
#' parametre_ht_dhp <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#'
#' # Mode stochastique, plusieurs années et plusieurs itérations
#' parametre_ht_dhp <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10, nb_step=5)
#'
param_ht <- function(fic_arbres, mode_simul='DET', nb_iter=1, nb_step=1, dt=10, seed_value=NULL){

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  #print("debut param_ht()")
  #fic_arbres=fic_arbres_test; nb_iter=10; mode_simul='STO'; nb_step=1; dt=10;
  #fic_arbres=data_arbre; mode_simul='STO'; nb_iter = 1; nb_step = 4; seed_value = 20;

  # liste des arbres
  liste_arbre <- fic_arbres %>% dplyr::select(id_pe, no_arbre) %>% unique()
  # liste des placettes
  liste_place <- unique(fic_arbres$id_pe)

  # faire une essence à la fois et les mettre dans des listes: une liste de listes
  tous <- list()
  for (ess in ht_liste_ess) {
    #ess='BOJ'

    # lecture des effets fixes pour l'essence
    # fichier rda ht_param_fixe
    param2_tr <- ht_param_fixe[[ess]] %>% dplyr::select(-essence)
    #print(param2_tr)
    # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
    data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
    # liste des arbres x nb_iter pour accueillir les erreurs résiduelles
    data_arbre <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, liste_arbre)))

    if (mode_simul=='STO') {
      # lecture des matrices de covariances des effets fixes de l'essence
      covparam <- ht_param_cov[[ess]]
      covparam$essence <- NULL

      # générer une série de paramètres d'effets fixes, une par itération par essence, qui sera utilisée pour tous les arbres de cette essence de toutes les placettes
      # il faut donc autant de séries qu'il y a d'itérations, les itérations sont en lignes, les effets fixes en colonne
      # pour que mvrnorm() fonctionne avec empirical=T, il faut au moins autant de n que la longueur du vecteur mu à simuler
      mu = as.matrix(param2_tr)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}
      param_ht = as.data.frame(matrix(mvrnorm(n = nb_iter_temp,
                                              mu = mu,
                                              Sigma = as.matrix(covparam),
                                              empirical = T
                                              ),
                                      nrow=nb_iter_temp))[1:nb_iter,]
      #print(param_ht)
      names(param_ht) <- names(param2_tr)
      param_ht <- param_ht %>%
        mutate(iter = row_number(), essence=ess)

      # fichier des effets aléatoires, corrélation et erreur résiduelle
      #ess<-'EPN'
      rand_ht <- ht_param_random[ht_param_random$essence==ess,] %>% dplyr::select(-essence)

      # générer un effet aléatoire de placette qui va s'appliquer sur le paramètre associé à ldhp2, le même pour tous les arbres de la placette
      # il faut donc autant d'effet aléatoire qu'il y a de placettes x nb_iter
      std2 <- rand_ht[rand_ht$CovParm=='ldhp2' & rand_ht$Subject=="placette", 1]
      #if (length(std$estimate)>0) { random_ldhp2 = as.data.frame(rnorm(nb_iter*length(liste_place), mean=0, sd = as.matrix(std))) }
      if (length(std2$estimate)>0) { random_ldhp2 = as.data.frame(mvrnorm(n=nb_iter*length(liste_place), mu=0, Sigma = as.matrix(std2), empirical = T)) }
      else{random_ldhp2 = as.data.frame(rep(0, nb_iter*length(liste_place)))} # le SAB n'a pas d'effet aléatoire
      #print(random_ldhp2)
      names(random_ldhp2) <- 'random_ldhp2'
      rand_plot = bind_cols(data_plot,random_ldhp2) %>% mutate(essence=ess)

      # générer une erreur résiduelle à l'échelle de l'arbre, donc autant de ligne que d'arbres  * nb_iter * time step
      std_res = sqrt(rand_ht[rand_ht$CovParm=='Residual', 1])$estimate  # ecart-type residuel
      rho = rand_ht[rand_ht$CovParm=='SP(POW)', 1]$estimate  # corrélation temporelle
      # fonction pour générer un element de la matrice de var-cov
      f <- function(i, j, std_res, rho, dt) { std_res^2 * rho^(abs(j-i)*dt) } # correlation sp(pow)
      # remplir la matrice de var-cov
      varcov = expand.grid(i=1:nb_step, j=1:nb_step)
      varcov = matrix(f(varcov$i, varcov$j, std_res, rho, dt), nrow=nb_step)
      n_arbre=length(liste_arbre$no_arbre)
      mu = rep(0,nb_step)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}
      res_arbre = as.data.frame(matrix(mvrnorm(n=nb_iter_temp*n_arbre, mu=mu, Sigma = varcov, empirical=T), nrow=nb_iter_temp*n_arbre))
      if (nb_step>1){res_arbre=res_arbre[1:(nb_iter*n_arbre),]}

      res_arbre = as.data.frame(bind_cols(data_arbre,res_arbre))
      # transposer les erreurs res pour avoir les time step en ligne
      res_arbre <- res_arbre %>%
        group_by(iter,id_pe, no_arbre) %>%
        pivot_longer(cols=contains('V'), names_to = "step", values_to = 'res_arbre') %>%
        mutate(step=as.numeric(substr(step,2,2))) %>%
        ungroup()
      #names(res_arbre) = c('iter',names(liste_arbre),'res_arbre')
      res_arbre$essence <- ess
    }
    else{
      # si mode déterministe
      param_ht <- param2_tr %>% mutate(iter=1, essence=ess)
      rand_plot <- 0
      res_arbre <- 0
    }
    temp <- list('essence'=ess, 'effet_fixe'=param_ht, 'random_placette'=rand_plot, 'erreur_residuelle'=res_arbre)
    tous <- append(tous, list(temp))
  }
  names(tous) <- ht_liste_ess
  return(tous)
}

