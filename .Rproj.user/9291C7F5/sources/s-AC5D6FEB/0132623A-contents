

test_that("param_ht() avec mode déterministe retourne les bons paramètres", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  parametre_ht_attendu <- readRDS(test_path("fixtures", "parametre_ht_attendu.rds"))
  parametre_ht <- param_ht(fic_arbres=data_arbre, mode_simul='DET')
  expect_equal(parametre_ht, parametre_ht_attendu)
})

test_that("param_ht() avec mode stochastique (seed=20) nb_step=1 retourne les bons paramètres", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  parametre_ht_attendu_sto <- readRDS(test_path("fixtures", "parametre_ht_attendu_sto.rds"))
  parametre_ht <- param_ht(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 1, seed_value = 20)
  expect_equal(parametre_ht, parametre_ht_attendu_sto)
})

test_that("param_ht() avec mode stochastique (seed=20) et nb_step=4 pour le meme arbre retourne les bonnes erreurs résiduelles", {
  data_arbre1 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1, no_mes=1)
  data_arbre2 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=11, nb_tige=1, no_arbre=1, no_mes=2)
  data_arbre3 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=12, nb_tige=1, no_arbre=1, no_mes=3)
  data_arbre4 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=13, nb_tige=1, no_arbre=1, no_mes=4)
  data_arbre <- bind_rows(data_arbre1, data_arbre2, data_arbre3, data_arbre4)

  parametre_ht <- param_ht(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 1, nb_step = 4, seed_value = 20)
  parametre_ht_erreur_res <- parametre_ht$ERS[[4]][[5]] # extraire les résidus

  expect_equal(parametre_ht_erreur_res, c(-0.66280133, -0.53694035, -1.32194235, -6.46956128))
})



