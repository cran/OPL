## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(OPL)

## -----------------------------------------------------------------------------
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

  x <- c("x1", "x2")  # le covariate
  y <- "y"            # la variabile dipendente
  w <- "w"            # la variabile di trattamento
  family <- "gaussian"  # Famiglia per glm
  ntree <- 100          # Numero di alberi per random forest
  mtry <- 2             # Numero di variabili da considerare in ogni split
  
result <- make_cate(model = "glm", train_data = train_data, test_data = test_data, w = w, x = x, y = y)

