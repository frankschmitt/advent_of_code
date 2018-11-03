#source("fractalart.R")

context("fractalart")

test_that("sets the initial grid", {
  expect_equal(".#.\n..#\n###", initial_grid())
})

test_that("constructing a 2x2 grid", {
  expect_equal("../.#", grid("../.#"))
})

test_that("constructing a rule for 2x2", {
  expect_equal(c("../.#", "##./#../..."), rule("../.# => ##./#../..."))
})

test_that("2x2 rule should match a 2x2 grid which is an exact match", {
  expect_equal(TRUE, matches(rule("../.# => ##./#../..."), grid("../.#")))
})

test_that("2x2 rule should not match a 2x2 grid which does not match", {
  expect_equal(FALSE, matches(rule("../.# => ##./#../..."), grid("../..")))
})


test_that("2x2 rule should not match 3x3 grid whose 2x2 subgrid is an exact match", {
  expect_equal(FALSE, matches(rule("../.# => ##./#../..."), grid(".../.#./...")))
})