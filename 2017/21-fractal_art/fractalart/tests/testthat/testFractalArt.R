#source("fractalart.R")

context("fractalart")

test_that("sets the initial grid", {
  expect_equal('.#.\n..#\n###', initial_grid())
})