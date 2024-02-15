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
