context("R6-help")

test_that('method documentation is recovered', {
  f <- function(x) { 'abcd'; 1:10 }
  expect_identical('abcd', get_method_doc(f))

  g <- function(x, y) {
    'efgh'
  }
  expect_identical('efgh', get_method_doc(g))

  h <- function(x) 1:10
  expect_identical(NULL, get_method_doc(h))
})

test_that('help prints methods documentation string', {
  TC <- R6Class("test_class",
                public = list(getx = function() { 'Gets x'; private$x },
                              setx = function(x) { 'Sets x'; self$x <- x },
                              one = function() 1,
                              help = R6_help),
                private = list(x = 1)
  )
  T <- TC$new()
  T$help('setx')
  T$help('getx')
  T$help('one')
  expect_error(T$help('abc'))
  expect_error(T$help('x'))
})