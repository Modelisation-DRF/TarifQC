test_that("cubage() avec mode déterministe et fichier param fourni estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_vol_attendu.rds"))
  DataVol <- cubage(fic_arbres=data_arbre, mode_simul = 'DET')
  expect_equal(DataVol, data_arbre_attendu)
})

test_that("cubage() avec mode stochastique estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_attendu_sto.rds"))
  data_arbre_attendu_sto <- readRDS(test_path("fixtures", "data_arbre_vol_attendu_sto.rds"))
  DataVol <- cubage(fic_arbres=data_arbre, mode_simul = 'STO', nb_iter = 200, nb_step=5, seed_value=20)
  expect_equal(DataVol, data_arbre_attendu_sto)
})

test_that("cubage() avec mode_simiul=STO sans les variables iter et step", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds")) %>% dplyr::select(-iter, -step)
  expect_error(cubage(fic_arbres=data_arbre, mode_simul='STO', nb_iter = 2, nb_step = 1))
})


test_that("cubage() avec un fichier de samare estime les bons volumes", {
  data_simul_samare <- readRDS(test_path("fixtures", "data_simul_samare.rds"))
  data_simul_samare_attendu <- readRDS(test_path("fixtures", "data_simul_samare_attendu.rds"))
  nb_iter <- max(data_simul_samare$iter)
  nb_step <- max(data_simul_samare$step)
  ht <- relation_h_d(fic_arbres = data_simul_samare, mode_simul = 'STO', nb_iter = nb_iter, nb_step = nb_step, reg_eco = T, dt=5, seed_value = 20)
  data_simul_samare_obtenu <- cubage(fic_arbres=ht, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step, seed_value = 20)
  expect_equal(data_simul_samare_obtenu$vol_dm3, data_simul_samare_attendu$vol_dm3)
})
