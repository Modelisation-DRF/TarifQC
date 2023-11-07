test_that("cubage() avec mode déterministe et fichier param fourni estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_vol_attendu.rds"))
  parametre_vol_attendu <- readRDS(test_path("fixtures", "param_vol_attendu.rds"))
  DataVol <- cubage(fic_arbres=data_arbre, mode_simul = 'DET', parametre_vol = parametre_vol_attendu)
  expect_equal(DataVol, data_arbre_attendu)
})

test_that("cubage() avec mode déterministe et fichier param non fourni estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_vol_attendu.rds"))
  DataVol <- cubage(fic_arbres=data_arbre, mode_simul = 'DET')
  expect_equal(DataVol, data_arbre_attendu)
})

test_that("cubage() avec mode stochastique estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  data_arbre_attendu_sto <- readRDS(test_path("fixtures", "data_arbre_vol_attendu_sto.rds"))
  parametre_vol_attendu_sto <- readRDS(test_path("fixtures", "param_vol_attendu_sto.rds"))
  DataVol <- cubage(fic_arbres=data_arbre, mode_simul = 'STO', iteration = 1, parametre_vol = parametre_vol_attendu_sto)
  expect_equal(DataVol, data_arbre_attendu_sto)
})
