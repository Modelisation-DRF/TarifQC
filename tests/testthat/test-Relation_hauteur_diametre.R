# relation_h_d(fic_arbres, mode_simul="DET")



test_that("relation_h_d() avec param par defaut estime les bonnes hauteurs", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_attendu.rds"))
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, data_arbre_attendu$hauteur_pred)
})

test_that("relation_h_d() avec param déterministe et utilisation du grouping_vars estime les bonnes hauteurs", {
    data_arbre1 <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='ERS', dhpcm=10, nb_tige=10, no_arbre=1, step=1)
    data_arbre2 <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='ERS', dhpcm=11, nb_tige=10, no_arbre=1, step=2)
    data_arbre3 <- data.frame(id_pe=2, veg_pot='MS2', sdom_bio="2E", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='SAB', dhpcm=12, nb_tige=10, no_arbre=1, step=1)
    data_arbre4 <- data.frame(id_pe=2, veg_pot='MS2', sdom_bio="2E", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='SAB', dhpcm=13, nb_tige=10, no_arbre=1, step=2)
    data_arbre <- bind_rows(data_arbre1,data_arbre2,data_arbre3,data_arbre4)
    DataHt <- relation_h_d(fic_arbres=data_arbre, grouping_vars = 'step')
    expect_equal(DataHt$hauteur_pred, c(8.9989156, 9.5695115, 9.0633389, 9.7025151))
})


# tester le stochastique quand il y plusieurs mesures par arbre
test_that("relation_h_d() avec mode stochastique (seed=20) et nb_step=4 pour le meme arbre retourne la bonne hauteur pour la mesure 2", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_attendu_sto.rds"))

  DataHt <- relation_h_d(fic_arbres=data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed=20)

  expect_equal(DataHt, data_arbre_attendu)
})

# tester le stochastique quand il n'y qu'une seule mesure par arbre
test_that("relation_h_d() avec mode stochastique avec seed=20 estime les bonnes hauteurs", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  data_arbre <- data_arbre %>% filter(step==1)
  data_arbre_attendu_sto1 <- readRDS(test_path("fixtures", "data_arbre_attendu_sto1.rds"))
  DataHt <- relation_h_d(fic_arbres=data_arbre, mode_simul = "STO", nb_iter = 200, nb_step=1, seed_value = 20)
  expect_equal(DataHt, data_arbre_attendu_sto1)
})



test_that("relation_h_d() avec mode_simiul=STO mais grouping_vars non nul retourne un message d'erreur", {
   data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
   expect_error(relation_h_d(fic_arbres=data_arbre, grouping_vars = 'step', mode_simul='STO'))
})

test_that("relation_h_d() avec mode_simiul=STO sans les variables iter et step", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds")) %>% dplyr::select(-iter, -step)
  expect_error(relation_h_d(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 2, nb_step = 1))
})


test_that("relation_h_d() avec essence non traitée retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='SAQ', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec veg_pot non traitée retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE9', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec milieu manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu=NA, p_tot=1000, t_ma=0.1, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec sdom_bio non traité retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="7E", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec p_tot manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=NA, t_ma=0.1, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})


test_that("relation_h_d() avec t_ma manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=NA, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})


test_that("relation_h_d() avec altitude manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=NA, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec essence manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='', dhpcm=10, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})


test_that("relation_h_d() avec dhpcm manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=NA, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec dhpcm <9 retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=8, nb_tige=1, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec nb_tige manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=NA, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec un seul arbre et nb_tige=0 retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=0, no_arbre=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  # avec une seul arbre dans la placette et nb_tige=0, la densité du peuplement est donc 0, donc le DQ moyenne ne se calcule pas (division par 0), donc dhpcm/dq_moy est à NaN, donc hauteur_pred est à NaN
  expect_equal(DataHt$hauteur_pred, NA)
})

test_that("relation_h_d() avec 2 arbres dont un avec nb_tige=0 retourne les bonnes hauteurs", {
  data_arbre1 <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=0, no_arbre=1)
  data_arbre2 <- data.frame(id_pe=1, veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=2)
  data_arbre <- bind_rows(data_arbre1, data_arbre2)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  # s'il y a au moins un arbre avec nb_tige>0, le DQ moyen va se calculer avec ceux >0, et puisque nb_tige n'entre pas directement dans la prédiction de la haut, une hauteur va se calculer pour tous les arbres
  expect_equal(DataHt$hauteur_pred, c(8.990205,8.990205))
})

test_that("relation_h_d() avec id_pe manquant retourne les bonnes hauteurs", {
  data_arbre1 <- data.frame(id_pe='', veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1)
  data_arbre2 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu='0', p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=2)
  data_arbre <- bind_rows(data_arbre1, data_arbre2)
  DataHt <- relation_h_d(fic_arbres=data_arbre)
  # si l'identifiant de placette est manquant pour certaines lignes, c'est arbres seront considérés comme faisant partis d'une meme placette dont l'identifiant est ''
  expect_equal(DataHt$hauteur_pred, c(8.990205,8.990205))
})


test_that("relation_h_d() avec reg_eco fonctionne", {
  data_arbre <- data.frame(id_pe=1, veg_pot='FE3', reg_eco="5e", milieu='0', p_tot=1000, t_ma=0.1, altitude=200, essence='ERS', dhpcm=10, nb_tige=10, no_arbre=1, step=1)
  DataHt <- relation_h_d(fic_arbres=data_arbre, mode_simul='DET', reg_eco = T)
  expect_equal(round(DataHt$hauteur_pred,2), round(9.237575,2))
})

# tester un fichier avec quelques arbres avec une ht fournie et d'autres à estimer: NON, la fonction va toujours estimer la hauteur de tous les arbres du fichier, ça sera à l'utilisateur ensuite de remplacer les ht_pred par les hauteur mesurées au besoin

# tester si le nombre d'itérations est le même dans le fichier des arbres que dans le fichier des paramètres:
# pas nécessaire, car en mode stochastique, on applique la fonction relation_h_d à une itération à la fois. Voir dans les exemples.
# Je n'ai pas voulu appliquer la relation_h_d à un fichier d'arbres qui contient toutes les itérations et toutes les step, avec un gros merge par iter/step,
# car si le fichier est gros, ça prends du temps.
# En plus, dans Natura, j'ai le fichier des arbres une seule itération à la fois. Je'aurais pu générer le fichiersdes arbres à la step 0 en multipliant
# les placettes par le nonmbre d'itérations, mais j'ai peur que ça ralentisse bcp et prenne bcp de mémoire d'avoir à travailler sur un gros fichier
# échelle arbre si plusieurs milliers de placettes



# la fct devrait être plus décortiquée
# il devrait y avoir une fct de base qui ne fait qu'appliquer l'équation à un seul arbre pour lequel on fournit les covariables nécessaires: ça serait bcp plus facile à tester
# puisqu'il y a 2 covariables qui dépendent des caractéristiques dendrométriques du peuplement, il pourrait aussi y avoir des fonctions qui applique la fct de base à chaque arbre du peuplement en calculant les caractéristiques
# mais je ne sais pas trop si c'est optimal quand on veut appliquer ça à tous les arbres de plusieurs placettes, car la prep se fait à l'échelle de la placette


