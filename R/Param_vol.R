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
#' @param fic_arbres Une table contenant la liste d'arbres dont le volume est à estimer. La table doit contenir les variables \code{no_arbre} (l'identifiant de l'arbre)
#' et \code{id_pe} (l'identifiant de la placette) et \code{essence} (le code d'essence). Si mode stochastique, la table doit aussi contenir les colonnes step et iter.
#'  Si pas de step, créer une colonne avec step=1.
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"}.
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer le volume pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param seed_value La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction. Optionnel.
#'
#' @return La fonction retourne une liste de deux éléments: une table pour les effets fixes et une pour l'erreur résiduelle et l'effet aléaoire de placette
#'
#' \enumerate{
#'   \item effet_fixe : une table avec 5 colonnes: 3 pour les paramètres des effets fixes de l'équation (b1, b2, b3), iter (numéro de l'itération) et essence (essence du modèle de volume). Une ligne par essence/iter.
#'   \item random: une table avec 31 colonnes: iter (numéro de l'itération), id_pe (identifiant de la placette), step (numéro de la step), random_plot (effet aléatoire de placette, 0 si mode déterministe) et une colonne pour chacune des 26 essences du modèle de volume contenant l'erreur résiduelle associée à cette essence, resid=0 si mode déterministe. Une ligne par placette/itération/step. 0 si \code{mode_simul="DET"}.
#' }
# #' @export
#'
#' @examples
#' # Mode déterministe
#' parametre_vol <- param_vol(fic_arbres=fic_arbres_test)
#'
#'#' # Mode stochastique, plusieurs années et 10 itérations
#' parametre_vol <- param_vol(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=10, nb_step=5)
#'
param_vol <- function(fic_arbres, mode_simul="DET", nb_iter=1, nb_step=1, seed_value=NULL){

  #fic_arbres=fic_artemis_sto; mode_simul='STO'; nb_iter=10; nb_step=5;
 # fic_arbres=fic_arbres; mode_simul=mode_simul; nb_iter=nb_iter; nb_step=nb_step; seed_value=seed_value;

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  # ne garder que les variables nécessaires si stochastique
  if (mode_simul=='STO'){
    fic_arbres <- fic_arbres %>% dplyr::select(id_pe, no_arbre, essence, iter, step) # si sto, on a besoin de iter et step
  }

  if (length(seed_value)>0) {set.seed(seed_value)}


  # établir la liste des essences pour lesquelles on a besoin du modele de volume
  fic_arbres_temp <- fic_arbres %>% left_join(tarif_ass_ess, by="essence")
  fic_arbres_temp2 <- fic_arbres_temp %>% rename(essence_orig=essence) %>% rename(essence=essence_volume)
  liste_ess_vol <- fic_arbres_temp2 %>% filter(!is.na(essence)) %>% dplyr::select(essence)
  liste_ess_vol <- unique(liste_ess_vol$essence)

  if (mode_simul=='STO') {

    # liste des arbres
    liste_arbre <- fic_arbres %>% dplyr::select(id_pe, no_arbre) %>% unique()

    # générer la liste de placettes
    liste_place <- unique(liste_arbre$id_pe)

    # Transposition des paramètres pour avoir une seule ligne
    param_tarif_tr <- tarif_param_fixe %>% pivot_wider(names_from = beta_ess, values_from = Estimate)

    # générer les effets fixes avec la matrice de covariances des effets fixes (tarif_param_cov.rda)
    # pour que mvrnorm() fonctionne avec empirical=T, il faut au moins autant de n que la longueur du vecteur mu à simuler
    mu = as.matrix(param_tarif_tr)
    l_mu = length(mu)
    if (nb_iter<l_mu) {nb_iter_temp=l_mu} else {nb_iter_temp=nb_iter}
    param_vol = as.data.frame(matrix(rockchalk::mvrnorm(n = nb_iter_temp, mu = mu, Sigma = as.matrix(tarif_param_cov), empirical = T),
                                     nrow=nb_iter_temp))[1:nb_iter,]
    names(param_vol) <- names(param_tarif_tr)
    param_vol <- param_vol %>% mutate(iter = row_number())

    # il faut retransposer pour utiliser les paramètres dans la fonction de cubage
    param_vol_tr <- param_vol %>%
      group_by(iter) %>%
      pivot_longer(cols=b1:b3_TIL, names_to = "parameter", values_to = "estimate") %>%
      separate_wider_delim(col=parameter, names=c("parm","essence"), delim='_', too_few = "align_start") %>%
      filter(!is.na(essence)) %>%
      group_by(iter, essence) %>%
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
    if (nb_iter<l_mu) {nb_iter_temp=l_mu} else {nb_iter_temp=nb_iter}
    rand = as.data.frame(matrix(rockchalk::mvrnorm(nb_iter_temp*length(liste_place), mu=mu, Sigma = sig, empirical=T), nrow=nb_iter_temp*length(liste_place)))
    if (nb_step>1) {rand <- rand[1:(nb_iter*length(liste_place)),]}
    rand = bind_cols(data_plot,rand)
    # transposer les effets aléatoires pour avoir les step en ligne
    rand <- rand %>%
      group_by(iter,id_pe) %>%
      pivot_longer(cols=contains('V'), names_to = "step", values_to = 'random_plot') %>%
      #mutate(step=as.numeric(substr(step,2,2))) %>% # ceci ne fonctionne pas avec step=10, ça plus que 1 caractère
      mutate(step=as.numeric(gsub('V','',step))) %>%
      ungroup()

    #générer les erreurs résiduelles qui sont fonction de l'essence pour chaque arbre
    sigma2_ess <- tarif_param_random[7:32,c(3,4)] %>% mutate(ess = substr(Group,9,11)) %>% dplyr::select(ess, -Group, Estimate)

    liste_ess <- sigma2_ess$ess
    res_tous <- NULL
    for (ess in liste_ess){
      #ess='ERS'
      #res = as.data.frame(rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(sigma2_ess[sigma2_ess$ess==ess,2]))))
      sig = diag(sigma2_ess[sigma2_ess$ess==ess,2], nrow=nb_step)
      mu=rep(0,nb_step)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}
      res = as.data.frame(matrix(rockchalk::mvrnorm(nb_iter_temp*length(liste_arbre$no_arbre), mu=mu, Sigma = sig, empirical = T), nrow=nb_iter_temp*length(liste_arbre$no_arbre)))
      if (nb_step>1){res=res[1:(nb_iter*length(liste_arbre$no_arbre)),]}


      res <- bind_cols(data_arbre,res)
      # transposer les erreurs pour avoir les step en ligne
      res <- res %>%
        group_by(iter,id_pe, no_arbre) %>%
        pivot_longer(cols=contains('V'), names_to = "step", values_to = ess) %>%
        #mutate(step=as.numeric(substr(step,2,2))) %>% # ceci ne fonctionne pas avec step=10, ça plus que 1 caractère
        mutate(step=as.numeric(gsub('V','',step))) %>%
        ungroup()
      if (is.null(res_tous)) {res_tous = res}
      else{res_tous <- left_join(res_tous, res, by=c('iter','id_pe','no_arbre','step'))}
    }
   # l'erreur résiduelle à choisir est celle de la colonne correspondant à l'essence de l'arbre


    # param_vol_tr : une ligne par iter/essence dans le modèle de vol donc 26 x nb_iter
    # rand:      une ligne par placette/iter/step
    # res_tous:  une ligne par placette/iter/step/arbre

    # merger random_plot et erreur residuelle
    param0 <- left_join(rand, res_tous, by=c('id_pe','iter','step'))
    # je ne peux pas y merger les effets fixe, le by ne peut pas fonctionner

  }
  else{ # si déterministe
    param_vol_tr <- tarif_param_fixe %>%
      separate_wider_delim(col=beta_ess, names=c("parm","essence"), delim='_', too_few = "align_start") %>%
      filter(!is.na(essence)) %>%
      group_by(essence) %>%
      pivot_wider(names_from = parm, values_from = Estimate) %>%
      mutate(b1 = tarif_param_fixe[tarif_param_fixe$beta_ess=='b1',2],
             #iter=1,
             random_plot=0,
             resid=0) %>%
      ungroup()

    param0 <- NULL

  }

  # ne garder que les essences de fic_arbres
  param_vol_tr <- param_vol_tr[param_vol_tr$essence %in% liste_ess_vol,]

  return(list('effet_fixe'=param_vol_tr, 'random'=param0))

}
