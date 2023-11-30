
test_that("param_vol() avec mode déterministe retourne les bons paramètres", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  parametre_vol_attendu <- readRDS(test_path("fixtures", "param_vol_attendu.rds"))
  parametre_vol <- param_vol(fic_arbres=data_arbre, mode_simul='DET')
  expect_equal(parametre_vol, parametre_vol_attendu)
})

test_that("param_vol() avec mode stochastique avec seed=20 retourne les bons paramètres des effets fixes", {
  # data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  # parametre_vol_attendu_sto <- readRDS(test_path("fixtures", "param_vol_attendu_sto.rds"))
  # parametre_vol_sto <- param_vol(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 1, seed_value = 20)
  # expect_equal(parametre_vol_sto, parametre_vol_attendu_sto)


  fic1 <- data.frame(id_pe='1', no_arbre=1:3)
  fic2 <- data.frame(id_pe='2', no_arbre=1:3)
  fic <- bind_rows(fic1,fic2)

  # la moyenne des sto doit donner le deterministe
  param_tarif_tr <- tarif_param_fixe %>% filter(beta_ess != 'b1') %>% mutate(essence_volume=substr(beta_ess,4,7),
                                                                             beta = substr(beta_ess,1,2)) %>%
    dplyr::select(-beta_ess) %>%
    group_by(essence_volume) %>%
    pivot_wider(names_from = beta, values_from = Estimate) %>% ungroup()
  param_tarif_tr$b1 <- tarif_param_fixe[tarif_param_fixe$beta_ess=='b1', "Estimate"]
  param_fixe <- round(as.data.frame(param_tarif_tr %>% dplyr::select(-essence_volume)),4)

  param_sto <- param_vol(fic_arbres=fic, mode_simul='STO', nb_iter=200, nb_step=5)

  param_fixe_sto = round(as.data.frame(bind_rows(lapply(1:200, function(x) {param_sto[[x]][[1]]})) %>% group_by(essence_volume) %>% dplyr::select(-iter) %>%
                                         summarise_all(mean) %>% arrange(essence_volume) %>%  dplyr::select(-essence_volume)),4)

  expect_true(all.equal(param_fixe, param_fixe_sto))

})


test_that("param_vol() avec mode stochastique (seed=20) retourne les bons effets aléatoires", {
  fic1 <- data.frame(id_pe='1', no_arbre=1:3)
  fic2 <- data.frame(id_pe='2', no_arbre=1:3)
  fic <- bind_rows(fic1,fic2)

  param_random = round(data.frame("random"=tarif_param_random[1,4]),4)


  param_sto <- param_vol(fic_arbres=fic, mode_simul='STO', nb_iter=200, nb_step=5, seed_value=20)

  param_mrandom_sto = round(as.data.frame(bind_rows(lapply(1:200, function(x) {param_sto[[x]][[2]]})) %>% dplyr::select(-iter, -id_pe, -step) %>%
                                            summarise(moy = mean(random_plot))),4)

  param_vrandom_sto = round(as.data.frame(bind_rows(lapply(1:200, function(x) {param_sto[[x]][[2]]})) %>%  dplyr::select(-iter, -id_pe, -step) %>%
                                            summarise(random=var(random_plot))),4)

  expect_true(all.equal(data.frame('moy'=0), param_mrandom_sto))
  expect_true(all.equal(param_random, param_vrandom_sto))

})

test_that("param_vol() avec mode stochastique (seed=20) retourne les bonnes erreurs résiduelles", {
  fic1 <- data.frame(id_pe='1', no_arbre=1:3)
  fic2 <- data.frame(id_pe='2', no_arbre=1:3)
  fic <- bind_rows(fic1,fic2)

  param_random = round(data.frame("random"=tarif_param_random[7:32,4]),4)


  param_sto <- param_vol(fic_arbres=fic, mode_simul='STO', nb_iter=200, nb_step=5, seed_value=20)

  param_mrandom_sto = round(as.data.frame(bind_rows(lapply(1:200, function(x) {param_sto[[x]][[3]]})) %>% dplyr::select(-iter, -id_pe, -step, -no_arbre) %>%
                                            summarise_all(mean)),4)

  param_vrandom_sto = round(as.data.frame(bind_rows(lapply(1:200, function(x) {param_sto[[x]][[3]]})) %>%  dplyr::select(-iter, -id_pe, -step, -no_arbre) %>%
                                            summarise_all(var) %>%
    pivot_longer(names_to = "essence", values_to = "random", cols = BOG:TIL) %>% dplyr::select(-essence)),4)


  expect_equal(sum(param_mrandom_sto),0)
  expect_true(all.equal(param_random, param_vrandom_sto))

})


test_that("param_vol() avec mode stochastique avec nb_iter=1 retourne un message d'erreur", {

  expect_error(param_vol(fic_arbres=fic, mode_simul='STO', nb_iter=1))

})
