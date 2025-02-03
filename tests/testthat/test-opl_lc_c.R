library(testthat)
library(tidyr)
library(OPL)
library(dplyr)
library(ggplot2)

test_that("Output dataset structure is correct", {
  # Mock input
  make_cate_result <- data.frame(
    my_cate = c(0.5, -0.3, 0.8, -0.1, 0.4),
    z1 = c(1, 2, 3, 4, 5),
    z2 = c(5, 4, 3, 2, 1),
    w = c(1, 0, 1, 0, 1)
  )
  z <- c("z1", "z2")
  w <- "w"

  # Run function
  result <- opl_lc_c(make_cate_result, z, w)

  # Check dataset structure
  expect_true(is.data.frame(result))
  expect_true(all(c("z1_std", "z2_std", "units_to_be_treated") %in% colnames(result)))
  expect_equal(nrow(result), nrow(make_cate_result))
})

test_that("Printed output includes main results", {
  # Mock input
  make_cate_result <- data.frame(
    my_cate = c(0.5, -0.3, 0.8, -0.1, 0.4),
    z1 = c(1, 2, 3, 4, 5),
    z2 = c(5, 4, 3, 2, 1),
    w = c(1, 0, 1, 0, 1)
  )
  z <- c("z1", "z2")
  w <- "w"

  # Capture printed output
  output <- capture.output(opl_lc_c(make_cate_result, z, w))

  # Check for key phrases in the output
  expect_true(any(grepl("Policy class: Linear combination", output)))
  expect_true(any(grepl("Average unconstrained welfare =", output)))
  expect_true(any(grepl("Percentage of treated =", output)))
})

test_that("Final table has correct values", {
  # Mock input
  make_cate_result <- data.frame(
    my_cate = c(0.5, -0.3, 0.8, -0.1, 0.4),
    z1 = c(1, 2, 3, 4, 5),
    z2 = c(5, 4, 3, 2, 1),
    w = c(1, 0, 1, 0, 1)
  )
  z <- c("z1", "z2")
  w <- "w"
  output <- capture.output(opl_lc_c(make_cate_result, z, w))

  # Cerca la sezione della tabella nell'output
  table_line <- grep("Not to treat|To treat", output, value = TRUE)

  # Verifica che la tabella sia presente
  expect_true(length(table_line) > 0)

  # Verifica contenuti specifici
  expect_true(any(grepl("Not to treat", table_line)))
  expect_true(any(grepl("To treat", table_line)))
})

