context("is")

test_that("is.R6 works as expected on R6 and R6Class objects", {
  AC <- R6Class("AC", public = list(x = 1))
  ac <- AC$new()

  expect_false(is.R6(AC))
  expect_true(is.R6(ac))
})

test_that("is.R6Class works as expected on R6 and R6Class objects", {
  AC <- R6Class("AC", public = list(x = 1))
  ac <- AC$new()

  expect_true(is.R6Class(AC))
  expect_false(is.R6Class(ac))
})

test_that("is.R6 and is.R6Class work as expected on non-R6 objects", {
  test_objects <- list(
    "int" = 1L,
    "int_vec" = rep(9L, 10),
    "char" = "hello",
    "charvec" = LETTERS,
    "data.frame" = data.frame(x = rnorm(10), y = rnorm(10)),
    "matrix" = matrix(runif(10), 2, 5)
  )
  for (obj in test_objects){
    expect_false(is.R6(obj))
    expect_false(is.R6Class(obj))
  }
})
