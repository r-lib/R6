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
