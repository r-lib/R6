test_that("Checking R6 class objects are recognized correctly by `is.R6()`", {
  Person <- R6Class("Person")
  Bob <- Person$new()
  expect_true(is.R6(Bob))
})

test_that("Checking R6 class generators are recognized correctly by `is.R6Class()`", {
  Person <- R6Class("Person")
  expect_true(is.R6Class(Person))
})
