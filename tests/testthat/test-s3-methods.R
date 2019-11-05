context("S3 methods")

test_that("`$` and `[[` methods don't interfere with R6 operations", {
  # Make sure that these method aren't used anywhere in internal R6 code
  `$.AC`  <- function(x, name) stop("Attempted to use `$.AC`")
  `[[.AC` <- function(x, name) stop("Attempted to use `[[.AC`")

  `$<-.AC`  <- function(x, name, value) stop("Attempted to use `$<-.AC`")
  `[[<-.AC` <- function(x, name, value) stop("Attempted to use `[[<-.AC`")


  AC <- R6Class("AC",
    public = list(
      x = 1,
      gety = function() private$y
    ),
    private = list(
      y = 2,
      y2 = function() y * 2
    ),
    active = list(
      z = function(value) 3
    )
  )

  expect_no_error(a <- AC$new())
  expect_no_error(b <- .subset2(a, "clone")())
})


test_that("Cloning avoids names() S3 method", {
  # A names() method can be defined for a class. We need to avoid it during
  # initialization and cloning -- need to use ls() instead, which does not get
  # dispatched based on class.
  names.A <- function(x) stop("Oops")

  A <- R6Class("A",
    public = list(x = 1),
    private = list(
      deep_clone = function(name, value) value
    )
  )

  expect_silent(a <- A$new())
  expect_silent(a1 <- a$clone())
  expect_silent(a2 <- a$clone(deep = TRUE))
})
