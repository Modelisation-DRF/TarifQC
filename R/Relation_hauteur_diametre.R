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
#' @description Estime la hauteur totale en mètre de chacun des arbres avec l'équation de Auger (2016). La fonction permet l'estimation pour une liste d'arbres regroupés en placette à une année donnée.
#' L'estimation peut être déterministe ou stochastique.
#'
#' @details
#' L'équation pour estimer la hauteur totale a été étalonnée avec un modèle linéaire mixte, par essence (Auger 2016).
#' Le modèle inclut un effet aléatoire de placette et une corrélation de type CorCar1 sur les erreurs résiduelles.
#' La fonction estime la hauteur de façon déterministe ou stochastique. Si l'estimation est stochastique, elle est pour une itération et une année données, la liste d'arbres fournie doit donc être pour une itération donnée et une année donnée.
#' Si \code{mode_simul}='STO', les paramètres doivent être générés préalablement avec la fonction \code{param_ht}. Voir les exemples.
#'
#' Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et
#' du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère
#' des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.
#'
#' @param fic_arbres Une table contenant la liste d'arbres regroupés en placettes (pour une itération et une année données) avec les informations suivantes:
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
#' }
#' @param mode_simul Mode de simulation : STO = stochastistique, DET = déterministe), par défaut "DET"
#' @param iteration Si \code{mode_simul}='STO', le numéro de l'itération à estimer (par défaut 1).
#' @param step Si \code{mode_simul}='STO', le numéro de l'année à estimer (par défaut 1, 1 à nb_step)
#' @param parametre_ht Si \code{mode_simul}='STO', l'objet retourné par la fonction \code{param_ht} contenant les paramètres pour le modèle de hauteur (pour toutes les itérations et steps).
#' @param grouping_vars Optionel. Si \code{mode_simul}='DET', les colonnes à ajouter comme variables de groupement, en plus de id_pe, pour calculer la surface terrière d'une placette.
#' Par exemple, si le fichier des arbres contient plus d'une année par arbre, ajouter le colonne identifiant l'année comme variable de groupement: grouping_vars='var1'.
#' S'il y a plusieurs variables de groupement: grouping_vars=c('var1','var2'). Ne peut pas être utilisé si \code{mode_simul}='STO'. Dans ce cas, créer un nouvel identifiant de la placette id_pe en concaténant toutes les variables de groupement.
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
#' # Exemple 3: STOCHASTIQUE: une seule année par arbre ---------------------------------------
#' # Générer les paramètres pour plusieurs itérations
#' parametre_ht <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#' # Estimer la hauteur pour l'itération 2
#' DataHt <- relation_h_d(fic_arbres=fic_arbres_test, mode_simul='STO', iteration=2, parametre_ht=parametre_ht)
#'
#' # Exemple 4: STOCHASTIQUE: plusieurs années par arbre ------------------------------------
#' # Générer les paramètres pour plusieurs itérations et années
#' parametre_ht <- param_ht(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=10, nb_step=5)
#' # Estimer la hauteur pour l'itération 2 et la step 3, où le 3 inclut la step 0, donc si la simulation aux pas de 10 ans a commencé en 2023, on veut l'estimation pour l'année 2043
#' DataHt <- relation_h_d(fic_arbres=fic_artemis_sto[fic_artemis_sto$iter==2 & fic_artemis_sto$annee==2023+((3-1)*10),], mode_simul='STO', iteration=2, step=3, parametre_ht=parametre_ht)
#'
#' # Exemple 5: STOCHASTIQUE: estimer toutes les itérations et toutes les années  ---------------------------
#' # Générer les paramètres du modèle de hauteur pour toutes les itérations et années
#' nb_iter <- length(unique(fic_artemis_sto$iter)) # 10
#' nb_step <- length(unique(fic_artemis_sto$annee)) # 5 (4 décennies + le point de départ en 2023)
#' parametre_ht <- param_ht(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#' # Appliquer le modèle de hauteur à chaque iteration/année
#' fic_artemis_final1 <- NULL
#' for (i in 1:nb_iter){
#'   for (k in 1:nb_step){
#'       ht <- relation_h_d(fic_arbres=fic_artemis_sto[fic_artemis_sto$iter==i & fic_artemis_sto$annee==2023+((k-1)*10),], mode_simul='STO', iteration=i, step=k, parametre_ht=parametre_ht)
#'       fic_artemis_final1 <- bind_rows(fic_artemis_final1, ht)
#'       }
#'    }
#'
#' # On peut aussi paralléliser les deux boucles for
#' nb_iter <- length(unique(fic_artemis_sto$iter)) # 10
#' nb_step <- length(unique(fic_artemis_sto$annee)) # 5 # 5 (4 décennies + le point de départ en 2023)
#' parametre_ht <- param_ht(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#' # Appliquer le modèle de hauteur à chaque iteration/step
#' registerDoFuture()
#' plan(multisession)
#' fic_artemis_final2 <- bind_rows(
#'   foreach (i = 1:nb_iter) %:% # nesting operator
#'       foreach (k = 1:nb_step) %dopar% {
#'             fic <- relation_h_d(fic_arbres=fic_artemis_sto[fic_artemis_sto$iter==i & fic_artemis_sto$annee==2023+((k-1)*10),], mode_simul='STO', iteration=i, step=k, parametre_ht=parametre_ht)
#'             }
#'  )
#'
relation_h_d<-function (fic_arbres, mode_simul="DET", iteration=1, step=1,  parametre_ht=NULL, grouping_vars=NULL, reg_eco=FALSE) {

  #fic_arbres=fic_arbres_test; mode_simul='STO'; iteration=1; step=1; parametre_ht=parametre_ht;
  #fic_arbres=fic_arbres_test; mode_simul='DET'; parametre_ht=parametre_ht; iteration=1; step=1; parametre_ht=NULL; grouping_vars=NULL;
  #fic_arbres=data_arbre[data_arbre$no_mes==2,]; mode_simul = "STO"; iteration = 1; step = 2; parametre_ht=parametre_ht;grouping_vars=NULL;

# le parametre grouping_vars ne peut etre utilisé avec le mode stochastique
if (mode_simul=='STO'){
  if (length(grouping_vars)>0) { stop("grouping_vars ne peut pas être utilisé avec mode_simul=STO")}
}

ii <- iteration
k <- step
grouping_vars <- c('id_pe', grouping_vars)

# si mode déterministe et que parametre_ht est vide, générer les paramètres
if (mode_simul=='DET' & length(parametre_ht)==0){
  parametre_ht <- param_ht(fic_arbres=fic_arbres, mode_simul='DET')
}

# fichier de paramètres de toutes les essences de l'itération i: les paramètres sont dans la liste de listes parametre_ht, une liste par essence, et pour chaque essence, une liste de 4 elements,
# dont 1: essence, 2: effets fixes, 3: effet aléatoire de placette sur ldhp2, 4: erreur residuelle

# effet fixes, une ligne par essence
param_ht_tr <- bind_rows(lapply(ht_liste_ess, function(x) parametre_ht[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter)))
# remplacer tous les NA par des 0
param_ht_tr <-  param_ht_tr %>% replace(is.na(.), 0)

# si reg_eco est fourni, faire l'association entre reg_eco et sdom_bio
if (reg_eco==TRUE){
  fic_arbres <- left_join(fic_arbres, regeco_ass_sdom[,c("reg_eco","sdom_bio")], by="reg_eco")
}

# compiler la st a la placette car on a besoin de sttot dans la relation h-d (avec les non commerciaux)
compil <- fic_arbres %>%
  group_by_at(vars(all_of(grouping_vars))) %>%
  summarise(sum_st_ha = sum(pi * (dhpcm/2/100)^2 * nb_tige * 25),
            dens = sum(nb_tige*25),
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

arbre2a <-arbre2 %>%
  mutate(essenceBck=essence,
         #essence=ifelse(is.na(essence)==TRUE,GrEspece,essence) # je dois enlever ça, ça n'a pas rapport, c'était quand le code était directement dans artemis
         ) %>%
  left_join(ht_ass_ess, by="essence") %>%
  rename(Essence_ori = essenceBck) %>%
  dplyr::select(-essence) %>%
  rename(essence=essence_hauteur)

# association des classes des variables categoriques selon l'essence au fichier des arbres
arbre3 <- left_join(arbre2a, ht_ass_pert, by = c("cl_perturb", "essence"))
arbre4 <- left_join(arbre3, ht_ass_mil, by = c("type_eco4", "essence"))
arbre5 <- left_join(arbre4, ht_ass_sd, by = c("sdom_bio", "essence"))
arbre6 <- left_join(arbre5, ht_ass_vp, by = c("veg_pot", "essence"))

# merger le fichier des parametres au fichier des arbres
arbre7 <- left_join(arbre6, param_ht_tr, by = "essence")

if (mode_simul=='STO'){
  # effet aléatoire: une ligne par placette
  rand_ldhp2 <- bind_rows(lapply(ht_liste_ess, function(x) parametre_ht[[x]]$random_placette %>% filter(iter==ii) %>% dplyr::select(-iter))) # le fichier random_placette a autant de lignes que de placette x nb_iter dans le fichier arbre, donc random_ldhp2 aura 27 x nb_placette
  # erreur résiduelle: une ligne par arbre
  resid <- bind_rows(lapply(ht_liste_ess, function(x) parametre_ht[[x]]$erreur_residuelle %>% filter(iter==ii, step==k) %>% dplyr::select(-iter,-step))) # le fichier erreur_residuelle a autant de lignes que le nombre d'arbres dans le fichier arbre x nb_iter x nb_step, donc resid aura 27 x nb arbre
  # merger le fichier des effets aléatoires de placette
  arbre7a <- left_join(arbre7, rand_ldhp2, by = c("id_pe","essence"))
  # merger le fichier des erreurs résiduelle arbres
  arbre7b <- left_join(arbre7a, resid, by = c("id_pe","no_arbre","essence"))
}
else{
  arbre7b <- arbre7 %>% mutate(res_arbre=0, random_ldhp2=0)
}

# appliquer l'equation
arbre8 <- arbre7b %>%
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
  rename(essence=Essence_ori, milieu=type_eco4, sdom_bio=sdom_orig)

return(arbre8)
}

