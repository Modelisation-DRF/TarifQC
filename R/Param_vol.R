################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#                                                              #
#   Utilise                                                    #
#   tarif_param_fixe.rda                                       #
#   tarif_param_cov.rda                                        #
#   tarif_param_random.rda                                     #
#                                                              #
################################################################

#' Génère les paramètres pour estimer le volume marchand sans écorce d'un arbre avec l'équation de Fortin et al. (2007)
#'
#' @description Génère les paramètres pour estimer le volume marchand sans écorce d'un arbre avec l'équation de Fortin et al. (2007)
#' Les paramètres peuvent être générés de façon déterministe ou stochastique.
#' Si le mode stochastique est utilisé, les paramètres seront générés pour tous les arbres/itétations/années.
#'
#' @details
#' L'équation pour estimer le volume marchand a été étalonnée avec un modèle linéaire mixte où l'essence est une covariable dans l'équation (Fortin et al. 2007).
#' Le modèle inclut un effet aléatoire de placette et un effet aléatoire de virée, mais seulement l'effet aléatoire de placette est simulé.
#'
#' Fortin, M., J. DeBlois, S. Bernier et G. Blais, 2007. Mise au point d’un tarif de cubage général pour
#' les forêts québécoises : une approche pour mieux évaluer l’incertitude associée aux prévisions.
#' For. Chron. 83: 754-765.
#'
#' @param fic_arbres Une table contenant la liste d'arbres dont le volume est à estimer. La table doit contenir les variables \code{no_arbre} (l'identifiant de l'arbre) et \code{id_pe} (l'identifiant de la placette).
#' @param mode_simul Le mode de simulation (STO = stochastiqie, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"}.
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer le volume pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param seed_value La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction. Optionnel.
#'
#' @return La fonction retourne une liste de listes, soit une liste par itération. Pour une itération, la liste contient 3 éléments:
#'
#' \enumerate{
#'   \item effet_fixe : une table avec 5 colonnes: 3 pour les paramètres des effets fixes de l'équation, numéro de l'itération et essence. Une ligne par essence.
#'   \item random_placette: une table avec 3 colonnes: effet aléatoire de placette, le numéro de l'itération et l'identifiant de la placette. Une ligne par placette/itération/step. 0 si \code{mode_simul="DET"}.
#'   \item erreur_residuelle : une table avec l'erreur résiduelle associée à chaque essence (une colonne par essence), identifiant de la placette, identifiant de l'arbre, et numéro de l'itération. Une ligne par arbre/placette/itération. 0 si \code{mode_simul="DET"}.
#' }
#' @export
#'
#' @examples
#' # Mode déterministe
#' parametre_vol <- param_vol(fic_arbres=fic_arbres_test)
#'
#' # Mode stochastique, pour une seule année et 10 itérations
#' parametre_vol <- param_vol(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#'
#'#' # Mode stochastique, plusieurs années et 10 itérations
#' parametre_vol <- param_vol(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10, nb_step=3)
#'
param_vol <- function(fic_arbres, mode_simul="DET", nb_iter=1, nb_step=1, seed_value=NULL){

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  #fic_arbres=fic_arbres_test; mode_simul='STO'; nb_iter=10; nb_step=1;


  if (length(seed_value)>0) {set.seed(seed_value)}

  # liste des arbres
  liste_arbre <- fic_arbres %>% dplyr::select(id_pe, no_arbre) %>% unique()

  # générer la liste de placettes
  liste_place <- unique(liste_arbre$id_pe)

  # lecture des paramètres des effets fixes (tarif_param_fixe.rda)
  param_tarif <- tarif_param_fixe

  if (mode_simul=='STO') {
    # Transposition des paramètres pour avoir une seule ligne
    param_tarif_tr <- param_tarif %>% pivot_wider(names_from = beta_ess, values_from = Estimate)

    # générer les effets fixes avec la matrice de covariances des effets fixes (tarif_param_cov.rda)
    # pour que mvrnorm() fonctionne avec empirical=T, il faut au moins autant de n que la longueur du vecteur mu à simuler
    mu = as.matrix(param_tarif_tr)
    l_mu = length(mu)
    if (nb_iter<l_mu) {nb_iter_temp=l_mu}
    else{nb_iter_temp=nb_iter}
    param_vol = as.data.frame(matrix(mvrnorm(n = nb_iter_temp, mu = mu, Sigma = as.matrix(tarif_param_cov), empirical = T),
                                     nrow=nb_iter_temp))[1:nb_iter,]
    names(param_vol) <- names(param_tarif_tr)
    param_vol <- param_vol %>% mutate(iter = row_number())

    # il faut retransposer pour utiliser les paramètres dans la fonction de cubage
    param_vol_tr <- param_vol %>%
      group_by(iter) %>%
      pivot_longer(cols=b1:b3_TIL, names_to = "parameter", values_to = "estimate") %>%
      separate_wider_delim(col=parameter, names=c("parm","essence_volume"), delim='_', too_few = "align_start") %>%
      filter(!is.na(essence_volume)) %>%
      group_by(iter, essence_volume) %>%
      pivot_wider(names_from = parm, values_from = estimate) %>%
      left_join(param_vol[,c('iter','b1')], by='iter') %>%
      ungroup()

    # fichier des effets aléatoires et erreur résiduelle
    # les 6 premières lignes sont les éléments de la matrice des effets aléatoires de virée et de placette (UN)
    # on va utiliser seulement celui de placette, donc la ligne 1 (UN(1,1)    j(i))
    # les lignes 7 à 32 sont la variance de l'erreur résiduelle pour chaque essence
    # tarif_param_random.rda

    # liste des placettes x nb_iter pour accueillir les effets aléatoires de placette à chaque itération
    data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
    # liste des arbres x nb_iter pour accueillir les erreurs résiduelles
    data_arbre <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, id_pe = liste_arbre)))
    names(data_arbre) <- c('iter', 'id_pe', 'no_arbre')

    # générer un effet aléatoire de placette pour chaque placette/iter/step
    #random_plot = data.frame('random_plot'=rnorm(nb_iter*length(liste_place), mean=0, sd = sqrt(as.matrix(tarif_param_random[1,4]))))
    sig = diag(tarif_param_random[1,4], nrow=nb_step)
    mu=rep(0,nb_step)
    l_mu = length(mu)
    if (nb_iter<l_mu) {nb_iter_temp=l_mu}
    else{nb_iter_temp=nb_iter}
    random_plot = as.data.frame(matrix(mvrnorm(nb_iter_temp*length(liste_place), mu=mu, Sigma = sig, empirical=T), nrow=nb_iter_temp*length(liste_place)))
    if (nb_step>1){random_plot=random_plot[1:(nb_iter*length(liste_place)),]}
    random_plot = bind_cols(data_plot,random_plot)
    # transposer les effets aléatoires pour avoir les step en ligne
    random_plot <- random_plot %>%
      group_by(iter,id_pe) %>%
      pivot_longer(cols=contains('V'), names_to = "step", values_to = 'random_plot') %>%
      mutate(step=as.numeric(substr(step,2,2))) %>%
      ungroup()

    #générer les erreurs résiduelles qui sont fonction de l'essence pour chaque arbre
    sigma2_ess <- tarif_param_random[7:32,c(3,4)] %>% mutate(ess = substr(Group,9,11)) %>% dplyr::select(ess, -Group, Estimate)

    liste_ess <- sigma2_ess$ess
    res_tous <- NULL
    for (ess in liste_ess){
      #ess='EPN'
      #res = as.data.frame(rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(sigma2_ess[sigma2_ess$ess==ess,2]))))
      sig = diag(sigma2_ess[sigma2_ess$ess==ess,2], nrow=nb_step)
      mu=rep(0,nb_step)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}
      res = as.data.frame(matrix(mvrnorm(nb_iter_temp*length(liste_arbre$no_arbre), mu=mu, Sigma = sig, empirical = T), nrow=nb_iter_temp*length(liste_arbre$no_arbre)))
      if (nb_step>1){res=res[1:(nb_iter*length(liste_arbre$no_arbre)),]}


      res <- bind_cols(data_arbre,res)
      # transposer les erreurs pour avoir les step en ligne
      res <- res %>%
        group_by(iter,id_pe, no_arbre) %>%
        pivot_longer(cols=contains('V'), names_to = "step", values_to = ess) %>%
        mutate(step=as.numeric(substr(step,2,2))) %>%
        ungroup()
      if (is.null(res_tous)) {res_tous = res}
      else{res_tous <- left_join(res_tous, res, by=c('iter','id_pe','no_arbre','step'))}
    }
    # res_tous <- bind_cols(data_arbre,res_tous)
    # l'erreur résiduelle à choisir est celle de la colonne correspondant à l'essence de l'arbre

    list_result <- list()
    for (i in 1:nb_iter) {
      list_result[[i]] <- list('effet_fixe'=param_vol_tr[param_vol_tr$iter==i,], 'random_placette'=random_plot[random_plot$iter==i,], 'erreur_residuelle'=res_tous[res_tous$iter==i,])
    }
  }
  else{ # si déterministe
    param_vol_tr <- param_tarif %>%
      separate_wider_delim(col=beta_ess, names=c("parm","essence_volume"), delim='_', too_few = "align_start") %>%
      filter(!is.na(essence_volume)) %>%
      group_by(essence_volume) %>%
      pivot_wider(names_from = parm, values_from = Estimate) %>%
      mutate(b1 = param_tarif[param_tarif$beta_ess=='b1',2], iter=1) %>%
      ungroup()

    random_plot <- 0
    res_tous <- 0

    list_result <- list()
    list_result[[1]] <- list('effet_fixe'=param_vol_tr, 'random_placette'=random_plot, 'erreur_residuelle'=res_tous)
  }
  #return(list('effet_fixe'=param_vol_tr, 'random_placette'=random_plot, 'erreur_residuelle'=res_tous))
  return(list_result)
}
