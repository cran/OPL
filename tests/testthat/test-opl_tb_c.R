library(tidyr)
library(OPL)
library(dplyr)
library(ggplot2)
test_that("opl_tb_c works correctly with simulated data", {

  # Preparazione dati di esempio
  set.seed(123)
  make_cate_result <- data.frame(
    my_cate = rnorm(100, mean = 0.1, sd = 0.5),
    z1 = runif(100, min = 0, max = 1),
    z2 = runif(100, min = 0, max = 1),
    w = sample(c(0, 1), 100, replace = TRUE) # Variabile w
  )

  # Definizione delle variabili z
  z <- c("z1", "z2")

  # Chiamata alla funzione con gli argomenti richiesti
  result <- opl_tb_c(make_cate_result = make_cate_result, z = z, w = "w")

  # Verifiche sui risultati
  expect_is(result, "data.frame") # Verifica che il risultato sia un data.frame
  expect_true("z1_std" %in% colnames(result)) # Verifica che i nomi delle colonne siano corretti
  expect_true("z2_std" %in% colnames(result)) # Verifica che le variabili standardizzate siano presenti
  expect_true("units_to_be_treated" %in% colnames(result)) # Verifica che la colonna sia creata

  # Verifica che le statistiche principali siano stampate
  expect_output(
    opl_tb_c(make_cate_result = make_cate_result, z = z, w = "w"),
    regexp = "Main results"
  )
})
