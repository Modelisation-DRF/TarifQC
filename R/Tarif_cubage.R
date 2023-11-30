################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function for estimating tree volume for a list of trees    #
#                                                              #
#                                                              #
#   Use data/tarif_ass_ess.rda                                 #
#                                                              #
################################################################


#' Estime le volume marchand sans écorce de chacun des arbres avec l'équation de Fortin et al. (2007)
#'
#' @description Estime le volume marchand sans écorce de chacun des arbres avec l'équation de Fortin et al. (2007). La fonction permet l'estimation pour une liste d'arbres regroupés en placette à une année donnée.
#' L'estimation peut être déterministe ou stochastique.
#'
#' @details
#' L'équation pour estimer le volume marchand a été étalonnée avec un modèle linéaire mixte où l'essence est une covariable dans l'équation (Fortin et al. (2007).
#' Le modèle inclut un effet aléatoire de placette et et un effet alétoire de virée, mais seul l'effet aléatoire de placette est simulé en mode stochastique.
#' La fonction estime le volume de façon déterministe ou stochastique. Si l'estimation est stochastique, elle est pour une itération et une année données , la liste d'arbres fournie doit donc être pour une itération et une année données.
#' Si \code{mode_simul}='STO', les paramètres doivent être générés préalablement avec la fonction \code{param_vol}. Voir les exemples.
#'
#' Fortin, M., J. DeBlois, S. Bernier et G. Blais, 2007. Mise au point d’un tarif de cubage général pour
#' les forêts québécoises : une approche pour mieux évaluer l’incertitude associée aux prévisions.
#' For. Chron. 83: 754-765.
#'
#' @param fic_arbres Une table contenant la liste d'arbres regroupés en placettes (pour une itération et une année données) avec les informations suivantes:
#' \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (>9 cm)
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item no_arbre: identifiant de l'arbre ou de la combinaison dhp/essence
#'    \item hauteur_pred: hauteur de l'arbre (m)
#' }
#' @param mode_simul Mode de simulation (STO = stochastic, DET = deterministic), par défaut "DET"
#' @param iteration Si \code{mode_simul}='STO', le numéro de l'iteration à estimer (par défaut 1)
#' @param step Si \code{mode_simul}='STO', le numéro de l'année à estimer (par défaut 1, 1 à nb_step)
#' @param parametre_vol Si \code{mode_simul}='STO', l'objet retourné par la fonction \code{param_vol} contenant les paramètres pour le modèle de volume (pour toutes les itérations).

#' @return La table \code{fic_arbres} avec une colonne contenant le volume marchand estimé en dm3 (vol_dm3).
#' @export
#'
#' @examples
#' # Exemple 1: DETERMINISTE: une seule année par arbre ----------------------------------------
#' # Estimer la hauteur et ensuite le volume
#' DataHt <- relation_h_d(fic_arbres=fic_arbres_test)
#' DataHtVol <- cubage(fic_arbres=DataHt)
#'
#' # Exemple 2: DETERMINISTE: plusieurs années par arbre -------------------------------------
#' # Estimer la hauteur et ensuite le volume
#' DataHt <- relation_h_d(fic_arbres=fic_artemis_det, grouping_vars = 'annee')
#' DataHtVol <- cubage(fic_arbres=DataHt)
#'
#' # Exemple 3: STOCHASTIQUE: une seul mesurage par arbre ---------------------------------------
#' # Générer les paramètres de hauteur volume pour plusieurs itérations
#' parametre_ht <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#' parametre_vol <- param_vol(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#' # Estimer la hauteur et le volume pour l'itération 2
#' DataHt <- relation_h_d(fic_arbres=fic_arbres_test, mode_simul='STO', iteration=2, parametre_ht=parametre_ht)
#' DataHtVol <- cubage(fic_arbres=DataHt, mode_simul='STO', iteration=2, parametre_vol=parametre_vol)
#'
#' # Exemple 4: STOCHASTIQUE: plusieurs mesurages par arbre --------------------------------------
#' # Générer les paramètres de hauteur volume pour plusieurs step et itérations
#' parametre_ht <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10, nb_step=5)
#' parametre_vol <- param_vol(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#' # Estimer la hauteur et le volume pour l'itération 2 et le pas de simulation 3
#' DataHt <- relation_h_d(fic_arbres=fic_artemis_sto[fic_artemis_sto$iter==2 & fic_artemis_sto$annee==2023+((3-1)*10),], mode_simul='STO', iteration=2, step=3, parametre_ht=parametre_ht)
#' DataHtVol <- cubage(fic_arbres=DataHt, mode_simul='STO', iteration=2, parametre_vol=parametre_vol)
#'
#' # Exemple 5: STOCHASTIQUE: traiter toutes les itérations et step -------------------------------
#' # Générer les paramètres de hauteur et volume pour toutes les itérations et time steps
#' nb_iter <- length(unique(fic_artemis_sto$iter)) # 10
#' nb_step <- length(unique(fic_artemis_sto$annee)) # 5 (donc 4 décennies + le point de départ en 2023)
#' parametre_ht <- param_ht(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#' parametre_vol <- param_vol(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter)
#' # Appliquer les modèles de hauteur et de volume à chaque iteration/step
#' fic_artemis_final1 <- NULL
#' for (i in 1:nb_iter){
#'   for (k in 1:nb_step){
#'       ht <- relation_h_d(fic_arbres=fic_artemis_sto[fic_artemis_sto$iter==i & fic_artemis_sto$annee==2023+((k-1)*10),], mode_simul='STO', iteration=i, step=k, parametre_ht=parametre_ht)
#'       vol <- cubage(fic_arbres=ht, mode_simul='STO', iteration=i, parametre_vol=parametre_vol)
#'       fic_artemis_final1 <- bind_rows(fic_artemis_final1, vol)
#'       }
#'    }
#'
#' # On peut aussi paralléliser les deux boucles for
#' nb_iter <- length(unique(fic_artemis_sto$iter)) # 10
#' nb_step <- length(unique(fic_artemis_sto$annee)) # 5 (donc 4 décennies + le point de départ en 2023)
#' parametre_ht <- param_ht(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#' parametre_vol <- param_vol(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter)
#' # Appliquer les modèles de hauteur et de volume à chaque iteration/step
#' registerDoFuture()
#' plan(multisession)
#' fic_artemis_final2 <- bind_rows(
#'   foreach (i = 1:nb_iter) %:% # nesting operator
#'       foreach (k = 1:nb_step) %dopar% {
#'             fic <- relation_h_d(fic_arbres=fic_artemis_sto[fic_artemis_sto$iter==i & fic_artemis_sto$annee==2023+((k-1)*10),], mode_simul='STO', iteration=i, step=k, parametre_ht=parametre_ht)
#'             fic <- cubage(fic_arbres=fic, mode_simul='STO', iteration=i, parametre_vol=parametre_vol)
#'             }
#'  )
cubage <- function (fic_arbres, mode_simul='DET', iteration=1, step=1, parametre_vol=NULL){

  #fic_arbres=DataHt; iteration=2; mode_simul='STO'; parametre_vol=parametre_vol;

  # si mode déterministe et que parametre_vol est vide, générer les paramètres
  if (mode_simul=='DET' &  length(parametre_vol)==0){
    parametre_vol <- param_vol(fic_arbres=fic_arbres, mode_simul='DET')
  }

ii <- iteration

# paramètres générés par la fct stochastique
param_tarif_tr <- parametre_vol[[ii]]$effet_fixe %>%  dplyr::select(-iter)

# association des essences aux essences du tarif de cubage (tarif_ass_ess.rda)
Essences_Volume <- tarif_ass_ess

arbre_vol <- fic_arbres %>% left_join(Essences_Volume, by="essence")

# Ajout des paramètres du tarif au fichier des arbres
arbre_vol2 <- left_join(arbre_vol, param_tarif_tr, by="essence_volume")

if (mode_simul=='STO'){
  random_plot <- parametre_vol[[ii]]$random_placette %>% dplyr::filter(step==step) %>% dplyr::select(-iter, -step) # un effet aléatoire de placettes pour tous les arbres de la placette
  resid <- parametre_vol[[ii]]$erreur_residuelle %>% dplyr::filter(step==step) %>% dplyr::select(-iter, -step) # une erreur résiduelle par arbre, selon l'essence (en colonne)
  # ajouter de l'effet aléatoire de placette à tous les arbres
  arbre_vol2a <- left_join(arbre_vol2, random_plot, by="id_pe")

  # ajouter de l'erreur résiduelle à chaque arbre et créer la variable resid  qui accueillera l'erreur de la bonne essence
  arbre_vol2b <- left_join(arbre_vol2a[c("id_pe","no_arbre","essence_volume")], resid, by= c("id_pe","no_arbre")) %>% mutate(resid=0)

  # garder la colonne de l'erreur residuelle de l'essence
  liste_ess <- unique(Essences_Volume$essence_volume) # liste des essences
  for (ess in liste_ess) {
   arbre_vol2b$resid[which(arbre_vol2b$essence_volume==ess)] <- as.matrix(arbre_vol2b[which(arbre_vol2b$essence_volume==ess),][ess])
  }
  arbre_vol2c <- inner_join(arbre_vol2a, arbre_vol2b[c("id_pe","no_arbre","resid")], by=c("id_pe","no_arbre")) %>% mutate(resid = as.numeric(resid))
}
else{
  arbre_vol2c <- arbre_vol2 %>% mutate(random_plot=0, resid=0)
}

# Calcul du volume

# volume en dm3;
# dhp en cm;
# hauteur en m;
# dres=1 pour résineux;
# ht_dhp = hauteur_pred/dhp;
# cylindre = pi*dhp**2*hauteur_pred/40;
# vol = -b1 x ht_dhp + (b2m + b3m*dres*dhp)*cylindre (mais le négatif est déjà appliqué au b1)

arbre_vol3 <- arbre_vol2c %>%
  mutate(
    dres = ifelse(essence_volume %in% c('EPB','EPN','EPR','MEL','PIB','PIG','PIR','PRU','SAB','THO'), 1, 0),
    cylindre = (pi * dhpcm*dhpcm * hauteur_pred)/40,
    ht_dhp = hauteur_pred/dhpcm,
    vol_dm3 = b1*ht_dhp + (b2+b3*dres*dhpcm)*cylindre + random_plot + resid,
    vol_dm3 = ifelse(vol_dm3<0, 4, vol_dm3)) %>% # 4 dm3 est le plus petit vol obs dans les données de calibration
  dplyr::select(-cylindre, -ht_dhp, -essence_volume, -b1,-b2,-b3, -random_plot, -resid, -dres)
# ça donne des NA pour tous les NC (il y a certain NC qui avait une hauteur, comme le PRP, mais n'ont pas de volume, ok)

return (arbre_vol3)
}






