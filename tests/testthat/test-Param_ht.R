

test_that("param_ht() avec mode déterministe retourne les bons paramètres", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  parametre_ht_attendu <- readRDS(test_path("fixtures", "parametre_ht_attendu.rds"))
  parametre_ht <- param_ht(fic_arbres=data_arbre, mode_simul='DET')
  expect_equal(parametre_ht, parametre_ht_attendu)
})

test_that("param_ht() avec mode stochastique (seed=20) nb_step=1 retourne les bons paramètres des effets fixes", {
  #data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  #parametre_ht_attendu_sto <- readRDS(test_path("fixtures", "parametre_ht_attendu_sto.rds"))
  #parametre_ht <- param_ht(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 1, seed_value = 20)
  #expect_equal(parametre_ht, parametre_ht_attendu_sto)

  fic1 <- data.frame(id_pe='1', no_arbre=1:3)
  fic2 <- data.frame(id_pe='2', no_arbre=1:3)
  fic <- bind_rows(fic1,fic2)

  # la moyenne des sto doit donner le deterministe
  param_fixe = round(as.data.frame(bind_rows(
    lapply(1:length(ht_param_fixe), function(x) {ht_param_fixe[[x]]})
    ) %>% arrange(essence) %>% dplyr::select(-essence)
    ),4)
  new_order = names(param_fixe)

  param_sto <- param_ht(fic_arbres=fic, mode_simul='STO', nb_iter=200, nb_step=5)


  param_fixe_sto = round(as.data.frame(bind_rows(lapply(1:length(param_sto), function(x) {param_sto[[x]][[2]]})) %>% group_by(essence) %>% dplyr::select(-iter) %>%
    summarise_all(mean) %>% arrange(essence) %>%  dplyr::select(-essence)) %>% dplyr::select(all_of(new_order)),4)


  #expect_equal(param_fixe_sto,param_sto)
  expect_true(all.equal(param_fixe, param_fixe_sto))

})

test_that("param_ht() avec mode stochastique (seed=20) retourne les bons effets aléatoires", {
  fic1 <- data.frame(id_pe='1', no_arbre=1:3)
  fic2 <- data.frame(id_pe='2', no_arbre=1:3)
  fic <- bind_rows(fic1,fic2)

  liste = unique(ht_param_random$essence)

  param_random = round(as.data.frame(bind_rows(
    lapply(liste, function(x) {ht_param_random[ht_param_random$essence==x & ht_param_random$CovParm=="ldhp2",]  })
  ) %>% arrange(essence) %>% dplyr::select(estimate)
  ),4)

  param_sto <- param_ht(fic_arbres=fic, mode_simul='STO', nb_iter=200, nb_step=5, seed_value=20)

  param_mrandom_sto = round(as.data.frame(bind_rows(lapply(1:length(param_sto), function(x) {param_sto[[x]][[3]]})) %>% group_by(essence) %>% dplyr::select(-iter, -id_pe) %>%
                                         summarise_all(mean) %>% arrange(essence) %>%  dplyr::select(-essence)),4)

  param_vrandom_sto = round(as.data.frame(bind_rows(lapply(1:length(param_sto), function(x) {param_sto[[x]][[3]]})) %>% group_by(essence) %>% dplyr::select(-iter, -id_pe) %>%
                                            summarise(estimate=var(random_ldhp2)) %>% arrange(essence) %>%  dplyr::select(-essence)) %>% filter(estimate>0),4)

  expect_true(all.equal(data.frame('random_ldhp2'=rep(0,27)), param_mrandom_sto))
  expect_true(all.equal(param_random, param_vrandom_sto))

})

test_that("param_ht() avec mode stochastique (seed=20) retourne les bonnes erreurs résiduelles", {
  # data_arbre1 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=10, nb_tige=1, no_arbre=1, no_mes=1)
  # data_arbre2 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=11, nb_tige=1, no_arbre=1, no_mes=2)
  # data_arbre3 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=12, nb_tige=1, no_arbre=1, no_mes=3)
  # data_arbre4 <- data.frame(id_pe='1', veg_pot='FE3', sdom_bio="1", milieu=0, p_tot=1000, t_ma=0.2, altitude=200, essence='ERS', dhpcm=13, nb_tige=1, no_arbre=1, no_mes=4)
  # data_arbre <- bind_rows(data_arbre1, data_arbre2, data_arbre3, data_arbre4)
  #
  # parametre_ht <- param_ht(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 1, nb_step = 4, seed_value = 20)
  # parametre_ht_erreur_res <- parametre_ht$ERS[[4]][[5]] # extraire les résidus
  #
  # expect_equal(parametre_ht_erreur_res, c(-0.66280133, -0.53694035, -1.32194235, -6.46956128))

  fic <- data.frame(id_pe='1', no_arbre=1:3)
  #fic2 <- data.frame(id_pe='2', no_arbre=1:3)
  #fic <- bind_rows(fic1,fic2)

  liste = unique(ht_param_random$essence)

  param_random = round(as.data.frame(bind_rows(
    lapply(liste, function(x) {ht_param_random[ht_param_random$essence==x & ht_param_random$CovParm=="Residual",]  })
  ) %>% arrange(essence) %>% dplyr::select(estimate)
  ),2)

  param_sto <- param_ht(fic_arbres=fic, mode_simul='STO', nb_iter=10000, nb_step=3, seed_value=20)

  param_mrandom_sto = round(as.data.frame(bind_rows(lapply(1:length(param_sto), function(x) {param_sto[[x]][[4]]})) %>% group_by(essence) %>% dplyr::select(-iter, -id_pe, -id_pe, -no_arbre, -step) %>%
                                            summarise(estimate=mean(res_arbre)) %>% arrange(essence) %>%dplyr::select(-essence)),4)

  param_vrandom_sto = round(as.data.frame(bind_rows(lapply(1:length(param_sto), function(x) {param_sto[[x]][[4]]})) %>% group_by(essence) %>% dplyr::select(-iter, -id_pe, -no_arbre, -step) %>%
                                            summarise(estimate=var(res_arbre)) %>% arrange(essence) %>% dplyr::select(-essence)),2)

  expect_true(all.equal(data.frame('estimate'=rep(0,27)), param_mrandom_sto))
  expect_true(all.equal(param_random, param_vrandom_sto))

})


test_that("param_ht() avec mode stochastique avec nb_iter=1 retourne un message d'erreur", {

  expect_error(param_ht(fic_arbres=fic, mode_simul='STO', nb_iter=1))

})


