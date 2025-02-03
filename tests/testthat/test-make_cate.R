library(testthat)
library(randomForest)
library(OPL)
library(dplyr)
# Crea i dati fittizi
set.seed(42)
train_data <- data.frame(
  y = rnorm(100),    # Outcome
  x1 = rnorm(100),   # Covariate
  x2 = rnorm(100),
  w = sample(0:1, 100, replace = TRUE)  # Trattamento
)

test_data <- data.frame(
  y = rnorm(100),    # Outcome
  x1 = rnorm(100),   # Covariate
  x2 = rnorm(100),
  w = sample(0:1, 100, replace = TRUE)  # Trattamento
)

test_that("make_cate funziona correttamente", {

  # Parametri da passare alla funzione
  x <- c("x1", "x2")  # le covariate
  y <- "y"            # la variabile dipendente
  w <- "w"            # la variabile di trattamento
  family <- "gaussian"  # Famiglia per glm
  ntree <- 100          # Numero di alberi per random forest
  mtry <- 2             # Numero di variabili da considerare in ogni split

  # Testa la funzione make_cate per il modello 'glm'
  result_glm <- make_cate(model = "glm", train_data = train_data, test_data = test_data, w = w, x = x, y = y)

  # Verifica che il risultato contenga la colonna my_cate
  expect_true("my_cate" %in% colnames(result_glm))

  # Testa la funzione make_cate per il modello 'rf'
  result_rf <- make_cate(model = "rf", train_data = train_data, test_data = test_data, w = w, x = x, y = y)

  # Verifica che il risultato contenga la colonna my_cate
  expect_true("my_cate" %in% colnames(result_rf))
})
