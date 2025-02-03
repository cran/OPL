library(testthat)
library(tidyr)
library(OPL)
library(dplyr)
library(ggplot2)
library(pander)


make_cate_result <- data.frame(
  my_cate = c(0.5, -0.3, 0.8, -0.1, 0.4),
  z1 = c(1, 2, 3, 4, 5),
  z2 = c(5, 4, 3, 2, 1),
  w = c(1, 0, 1, 0, 1)
)
z <- c("z1", "z2")
w <- "w"

# Chiamata alla funzione con gli argomenti richiesti
result <- opl_dt_c(make_cate_result = make_cate_result, z = z, w = "w")

# Creiamo un esempio con comb_name e ris simulati per testare
comb_name <- data.frame(
  Variable1 = c("z1", "z1", "z2"),
  Variable2 = c("z2", "z2", "z1"),
  Variable3 = c("z1", "z2", "z2"),
  Avg_Constrained_Welfare = c(0.7, 0.8, 0.9)
)

ris <- c(0.8, 0.9, 0.85)  # Welfare vincolato

# Chiamata alla funzione da testare
output <- paste(
  "----------------------------------\n",
  "- Results on selection variables -\n",
  "----------------------------------\n",
  "Selection variables and Avg Constrained Welfare\n",
  paste(capture.output(pander(comb_name)), collapse = "\n"),
  "                                \n",
  "                                \n",
  "--------------------------------\n",
  "-  Max Avg Constrained Welfare -\n",
  "--------------------------------\n",
  paste(capture.output(pander(comb_name[which(comb_name[, 4] == max(round(ris, 7))),])), collapse = "\n"),
  "                                \n",
  sep = ""
)

# Test sulla struttura dell'output
test_that("Output format is correct", {
  # Controllo che l'output contenga il testo previsto
  expect_true(grepl("Results on selection variables", output))
  expect_true(grepl("Max Avg Constrained Welfare", output))

  # Verifica che le informazioni sulle variabili siano correttamente incluse
  expect_true(grepl("Variable1", output))
  expect_true(grepl("Variable2", output))
  expect_true(grepl("Variable3", output))
  expect_true(grepl("Avg_Constrained_Welfare", output))

  # Verifica che il welfare massimo sia identificato correttamente
  expect_true(grepl("Max Avg Constrained Welfare", output))
  expect_true(grepl("0.9", output))  # Welfare massimo simulato
})

