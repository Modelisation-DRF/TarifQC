################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#                                                              #
#  Utilise                                                     #
#       ht_ass_ess.rda                                         #
#       ht_ass_mil.rda                                         #
#       ht_ass_vp.rda                                          #
#       ht_ass_sd.rda                                          #
#       ht_ass_pert.rda                                        #
#       ht_liste_ess.rda                                       #
#       regeco_ass_sdom.rda                                    #
#                                                              #
################################################################


#' Estime la hauteur totale de chacun des arbres avec l'équation de Auger (2016).
#'
#' @description Estime la hauteur totale en mètre de chacun des arbres avec l'équation de Auger (2016). La fonction permet l'estimation pour une liste d'arbres regroupés en placette.
#' L'estimation peut être déterministe ou stochastique.
#'
#' @details
#' L'équation pour estimer la hauteur totale a été étalonnée avec un modèle linéaire mixte, par essence (Auger 2016).
#' Le modèle inclut un effet aléatoire de placette et une corrélation de type CorCar1 sur les erreurs résiduelles.
#' La fonction estime la hauteur de façon déterministe ou stochastique. Si \code{mode_simul}='STO', les paramètres doivent être générés préalablement avec la fonction \code{param_ht}.
#' Voir les exemples.
#'
#' Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et
#' du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère
#' des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.
#'
#' @param fic_arbres Une table contenant la liste d'arbres regroupés au minimum en placettes avec les informations suivantes:
#' \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (>9 cm)
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item no_arbre: identifiant de l'arbre ou de la combinaison dhp/essence, nécessaire seulement si \code{mode_simul}='STO'
#'    \item nb_tige: nombre d'arbres de l'essence et de la classe de dhp, dans 400 m2 (pour calculer la surface terrière de la placette)
#'    \item sdom_bio: sous-domaine bioclimatique, en majuscule (ex: 1, 2E, 4O), seuls les domaines 1 à 6 sont traités
#'    \item veg_pot: code de végétation potentielle, 3 premiers caractères du type écologique  (ex: FE3, MS2)
#'    \item milieu: 4e caractère du type écologique, doit être caractère, une valeur de 0 à 9 (ex: 2)
#'    \item p_tot: précipitation totale annuelle moyenne sur la période 1980-2010 (mm)
#'    \item t_ma: température annuelle moyenne sur la période 1980-2010 (Celcius)
#'    \item altitude: altitude (m)
#'    \item reg_eco: Optionnel. Code de la région écologique. Vous pouvez fournir la région au lieu du sous-domaine, alors mettre le paramètre \code{reg_eco=TRUE}
#'    \item iter: numéro de l'itération, seulement si mode stochastique, doit être numéroté de 1 à nb_iter
#'    \item step: numéro de la step, seulement si mode stochastique, doit être numéroté de 1 à nb_step. Obligatoire même si le fichier n'est qu'une liste d'arbres à un moment donné.
#' }
#' @param mode_simul Mode de simulation : STO = stochastistique, DET = déterministe), par défaut "DET"
#' @param grouping_vars Optionel. Si \code{mode_simul}='DET', les colonnes à ajouter comme variables de groupement, en plus de id_pe, pour calculer la surface terrière d'une placette.
#' Par exemple, si le fichier des arbres contient plus d'une année par arbre, ajouter le colonne identifiant l'année comme variable de groupement: grouping_vars='var1'.
#' S'il y a plusieurs variables de groupement: grouping_vars=c('var1','var2').
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer la hauteur pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param dt La durée de l'intervalle de temps entre deux mesures d'un même arbre si \code{nb_step>1} (par défaut 10), ignoré si \code{mode_simul="DET"}.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @param reg_eco Optionel. Mettre à \code{TRUE} si reg_eco est fourni dans \code{fic_arbres} au lieu de sdom_bio. reg_eco sera converti en sdom_bio. La colonne sdom_bio ne doit pas être dans \code{fic_arbres}.
#'
#' @return La table \code{fic_arbres} avec une colonne contenant la hauteur estimée en mètres (hauteur_pred).
#' @export
#'
#' @examples
#' # Exemple 1: DETERMINISTE: un seule année par arbre ---------------------------------------
#' DataHt <- relation_h_d(fic_arbres=fic_arbres_test)
#'
#' # Exemple 2: DETERMINISTE: avec grouping_vars, plusieurs années par arbre ----------------
#' DataHt <- relation_h_d(fic_arbres=fic_artemis_det, grouping_vars='annee')
#'
#' # Exemple 3: STOCHASTIQUE: plusieurs années par arbre ------------------------------------
#' nb_iter <- length(unique(fic_artemis_sto$iter))
#' nb_step <- length(unique(fic_artemis_sto$step))
#' DataHt <- relation_h_d(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#'
relation_h_d<-function (fic_arbres, mode_simul="DET", nb_iter=1, nb_step=1, dt=10, seed_value=NULL, grouping_vars=NULL, reg_eco=FALSE) {

  #fic_arbres=fic_artemis_sto; mode_simul='STO'; nb_iter=10; nb_step=5; dt=10;
  #fic_arbres=fic_arbres_test; mode_simul='DET'; grouping_vars=NULL; nb_iter=1; nb_step=1; dt=10;
  #fic_arbres=data_arbre; mode_simul='DET'; grouping_vars=NULL; nb_iter=1; nb_step=1; dt=10;
  # fic_arbres = SimulHtVol1; mode_simul = 'STO'; nb_iter = nb_iter; nb_step = nb_step; reg_eco = T; dt=5; grouping_vars=NULL; seed_value=NULL;
# fic_arbres = data_simul_samare; mode_simul = 'STO'; nb_iter = nb_iter; nb_step = nb_step; reg_eco = T; dt=5; seed_value = 20;
# fic_arbres = data_simul_samare4; mode_simul = 'STO'; nb_iter = nb_iter; nb_step = nb_step; reg_eco = T; dt=5; seed_value = NULL;

  # le parametre grouping_vars ne peut pas etre utilisé avec le mode stochastique
  # en mode stochastique, les variables iter et step sont obligatoires
  if (mode_simul=='STO'){
    if (length(grouping_vars)>0) { stop("grouping_vars ne peut pas être utilisé avec mode_simul=STO")}
    if (length(setdiff(c("iter","step"), names(fic_arbres))) >0) { stop("les colonnes iter et step doivent être dans fic_arbres avec mode_simul=STO")}
  }

  if (mode_simul=='STO'){ grouping_vars <- c('id_pe', "iter", "step")}
  if (mode_simul=='DET'){ grouping_vars <- c('id_pe', grouping_vars)}

# générer les paramètres de la relation h_d
parametre_ht <- param_ht(fic_arbres=fic_arbres, mode_simul=mode_simul, nb_iter=nb_iter, nb_step=nb_step, dt=dt, seed_value=seed_value)



# si reg_eco est fourni, faire l'association entre reg_eco et sdom_bio
if (reg_eco==TRUE){
  fic_arbres <- left_join(fic_arbres, regeco_ass_sdom[,c("reg_eco","sdom_bio")], by="reg_eco")
}

# compiler la st a la placette car on a besoin de sttot dans la relation h-d (avec les non commerciaux)
# ajout de l'ptopn , na.rm = T pour calculer les moyenne par placette même s'il y a des dhpcm à NA dans la placette (comme dans le cas où on met des morts dans le fichier)
compil <- fic_arbres %>%
  group_by_at(vars(all_of(grouping_vars))) %>%
  summarise(sum_st_ha = sum(pi * (dhpcm/2/100)^2 * nb_tige * 25, na.rm = T),
            dens = sum(nb_tige*25, na.rm = T),
            dhp_moy = sqrt((sum_st_ha*40000)/(dens*pi)))

# ajouter la st au fichier des arbres et preparer les autres variables necessaires
arbre2 <- inner_join(fic_arbres, compil, by = grouping_vars) %>%
  rename(type_eco4=milieu) %>%
  mutate(sdom_orig = sdom_bio,
         logdhp = log(dhpcm+1),
         cl_perturb="NON",
         rdhp = dhpcm/dhp_moy,
         sdom_bio = ifelse(sdom_bio == '1', "1OUEST",
                           ifelse(sdom_bio=='2E',"2EST",
                                  ifelse(sdom_bio=="2O","2OUEST",
                                         ifelse(sdom_bio=='3E','3EST',
                                                ifelse(sdom_bio=='3O',"3OUEST",
                                                       ifelse(sdom_bio=='4E',"4EST",
                                                              ifelse(sdom_bio=="4O","4OUEST",
                                                                     ifelse(sdom_bio=='5E','5EST',
                                                                            ifelse(sdom_bio=='5O',"5OUEST",
                                                                                   ifelse(sdom_bio=='6E','6EST',
                                                                                          ifelse(sdom_bio=='6O',"6OUEST",
                                                                                                 NA))))))))))))

# ajouter l'essence associée au modèle de hauteur
arbre2a <- arbre2 %>%
  left_join(ht_ass_ess, by="essence") %>%
  rename(essence_orig = essence) %>%
  rename(essence=essence_hauteur)

# association des classes des variables categoriques selon l'essence au fichier des arbres
arbre3 <- left_join(arbre2a, ht_ass_pert, by = c("cl_perturb", "essence"))
arbre4 <- left_join(arbre3, ht_ass_mil, by = c("type_eco4", "essence"))
arbre5 <- left_join(arbre4, ht_ass_sd, by = c("sdom_bio", "essence"))
arbre6 <- left_join(arbre5, ht_ass_vp, by = c("veg_pot", "essence"))

# merger le fichier des parametres au fichier des arbres
if (mode_simul=='DET') {
  arbre7 <- left_join(arbre6, parametre_ht, by = c("essence")) # quelques secondes
}
if (mode_simul=='STO') {
  arbre7 <- left_join(arbre6, parametre_ht, by = c("id_pe", "no_arbre", "essence", "iter", "step")) # quelques secondes
  # ici le merge ne fonctionne pas quand les variables du peuplement sont dans les deux fichiers
}

# appliquer l'equation
arbre8 <- arbre7 %>%
  ungroup() %>%
  mutate(eq_ldhp = ef_ldhp * logdhp,
         eq_alt = ef_alt * altitude * logdhp,
         eq_ptot = ef_ptot * p_tot * logdhp,
         eq_tmoy = ef_tmoy * t_ma * logdhp,
         eq_st = ef_st * sum_st_ha * logdhp,
         eq_rdhp = ef_rdhp * rdhp * logdhp,
         eq_ldhp2 = (ef_ldhp2+random_ldhp2) * logdhp * logdhp) %>%
  mutate(eq_pert = ifelse(pert=='NON', pert_NON * logdhp,
                          ifelse(pert=='INT', pert_INT * logdhp,
                                 ifelse(pert=='MOY', pert_MOY * logdhp,
                                        NA))),
         eq_sdom = ifelse(sdom=='1', sd_1 * logdhp,
                          ifelse(sdom=='2EST', sd_2EST * logdhp,
                                 ifelse(sdom=='2OUEST', sd_2OUEST * logdhp,
                                        ifelse(sdom=='3EST', sd_3EST * logdhp,
                                               ifelse(sdom=='3OUEST', sd_3OUEST * logdhp,
                                                      ifelse(sdom=='4EST', sd_4EST * logdhp,
                                                             ifelse(sdom=='4OUEST', sd_4OUEST * logdhp,
                                                                    ifelse(sdom=='5EST', sd_5EST * logdhp,
                                                                           ifelse(sdom=='5OUEST', sd_5OUEST * logdhp,
                                                                                  ifelse(sdom=='6EST', sd_6EST * logdhp,
                                                                                         ifelse(sdom=='6OUEST', sd_6OUEST * logdhp,
                                                                                                NA))))))))))),
         eq_mil = ifelse(milieu=='0', mil_0 * logdhp,
                         ifelse(milieu=='1', mil_1 * logdhp,
                                ifelse(milieu=='2', mil_2 * logdhp,
                                       ifelse(milieu=='3', mil_3 * logdhp,
                                              ifelse(milieu=='4', mil_4 * logdhp,
                                                     ifelse(milieu=='5', mil_5 * logdhp,
                                                            ifelse(milieu=='6', mil_6 * logdhp,
                                                                   ifelse(milieu=='7', mil_7 * logdhp,
                                                                          ifelse(milieu=='8', mil_8 * logdhp,
                                                                                 ifelse(milieu=='9', mil_9 * logdhp,
                                                                                        NA)))))))))),
         eq_vp = ifelse(vp=='FC1', vp_FC1 * logdhp,
                    ifelse(vp=='FE1', vp_FE1 * logdhp,
                        ifelse(vp=='FE2', vp_FE2 * logdhp,
                               ifelse(vp=='FE3', vp_FE3 * logdhp,
                                      ifelse(vp=='FE4', vp_FE4 * logdhp,
                                             ifelse(vp=='FE5', vp_FE5 * logdhp,
                                                    ifelse(vp=='FE6', vp_FE6 * logdhp,
                                                       ifelse(vp=='FO1', vp_FO1 * logdhp,
                                                           ifelse(vp=='ME1', vp_ME1 * logdhp,
                                                                  ifelse(vp=='MF1', vp_MF1 * logdhp,
                                                                         ifelse(vp=='MJ1', vp_MJ1 * logdhp,
                                                                                ifelse(vp=='MJ2', vp_MJ2 * logdhp,
                                                                                       ifelse(vp=='MS1', vp_MS1 * logdhp,
                                                                                              ifelse(vp=='XS2' & essence=='SAB', vp_XS2 * logdhp,
                                                                                                  ifelse(vp=='MS2' & essence!='SAB', vp_MS2 * logdhp,
                                                                                                     ifelse(vp=='MS4', vp_MS4 * logdhp,
                                                                                                            ifelse(vp=='MS6', vp_MS6 * logdhp,
                                                                                                                   ifelse(vp=='RB1', vp_RB1 * logdhp,
                                                                                                                          ifelse(vp=='RB5', vp_RB5 * logdhp,
                                                                                                                                 ifelse(vp=='RC3', vp_RC3 * logdhp,
                                                                                                                                        ifelse(vp=='RE1', vp_RE1 * logdhp,
                                                                                                                                               ifelse(vp=='RE2', vp_RE2 * logdhp,
                                                                                                                                                      ifelse(vp=='RE3', vp_RE3 * logdhp,
                                                                                                                                                             ifelse(vp=='RE4', vp_RE4 * logdhp,
                                                                                                                                                                    ifelse(vp=='RP1', vp_RP1 * logdhp,
                                                                                                                                                                           ifelse(vp=='RS1', vp_RS1 * logdhp,
                                                                                                                                                                                  ifelse(vp=='RS2', vp_RS2 * logdhp,
                                                                                                                                                                                         ifelse(vp=='RS3', vp_RS3 * logdhp,
                                                                                                                                                                                                ifelse(vp=='RS4', vp_RS4 * logdhp,
                                                                                                                                                                                                       ifelse(vp=='RS5', vp_RS5 * logdhp,
                                                                                                                                                                                                              ifelse(vp=='RT1', vp_RT1 * logdhp,
                                                                                                                                                                                                                     NA)))))))))))))))))))))))))))))))
  ) %>%
  mutate(hauteur_pred = ifelse(!is.na(essence) & dhpcm>9, 1.3 + res_arbre + eq_ldhp + eq_alt + eq_ptot + eq_tmoy + eq_st + eq_rdhp + eq_pert + eq_sdom + eq_mil + eq_vp + eq_ldhp2, NA),
         hauteur_pred = ifelse(hauteur_pred<1.3, 1.3, hauteur_pred)) %>%
  dplyr::select(-res_arbre, -random_ldhp2, -dens, -dhp_moy, -contains("sd_"), -contains("pert_"), -contains("mil_"), -contains("vp_"), -contains("ef_"), -contains("eq_"), -logdhp, -rdhp, -sum_st_ha, -pert, -milieu, -vp, -sdom, -essence, -cl_perturb, -sdom_bio) %>%
  rename(essence=essence_orig, milieu=type_eco4, sdom_bio=sdom_orig)

return(arbre8)
}

