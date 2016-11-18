context("initialize")

test_that("no initializer", {
  NoInitializer <- R6Class("NoInitializer")

  # Absence of initializer means we might use an inherited initializer,
  # but we don't know its interface at the time the class is created (#12).
  expect_identical(formals(NoInitializer$new), as.pairlist(alist(... = )))
})

test_that("empty initializer", {
  EmptyInitializer <- R6Class("EmptyInitializer", public = list(initialize = function() NULL))
  expect_null(formals(EmptyInitializer$new))
})

test_that("initializer with args", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = )))
})

test_that("initializer with args and defaults", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a = "", b) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = "", b = )))
})

test_that("initializer with dots", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a = "", ..., b) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = "", ... = , b = )))
})

test_that("inherited initializer", {
  A <- R6Class("A", public = list(initialize = function(a = "", ..., b) NULL))
  B <- R6Class("B", inherit = A)
  expect_identical(formals(B$new), as.pairlist(alist(... = )))
})
