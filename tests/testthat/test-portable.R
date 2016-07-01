context("portable")

test_that("initialization", {
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 1,
      initialize = function(x, y) {
        self$x <- self$getx() + x    # Assign to self; also access a method
        private$y <- y          # Assign to private
      },
      getx = function() self$x,
      gety = function() private$y
    ),
    private = list(
      y = 2
    )
  )
  A <- AC$new(2, 3)
  expect_identical(A$x, 3)
  expect_identical(A$gety(), 3)

  # No initialize method: throw error if arguments are passed in
  AC <- R6Class("AC", portable = TRUE, public = list(x = 1))
  expect_error(AC$new(3))
})


test_that("empty members and methods are allowed", {
  # No initialize method: throw error if arguments are passed in
  AC <- R6Class("AC", portable = TRUE)
  expect_no_error(AC$new())
})


test_that("Private members are private, and self/private environments", {
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 1,
      gety = function() private$y,
      getx = function() self$x,
      getx2 = function() private$getx_priv(),
      getself = function() self,
      getprivate = function() private
    ),
    private = list(
      y = 2,
      getx_priv = function() self$x
    )
  )
  A <- AC$new()

  # Environment structure
  expect_identical(A$getself(), A)
  expect_identical(parent.env(A), emptyenv())

  # The private binding environment contains private fields
  private_bind_env <- A$getprivate()
  expect_identical(ls(private_bind_env), c("getx_priv", "y"))
  expect_identical(parent.env(private_bind_env), emptyenv())

  # Eval environment for public methods
  eval_env <- environment(A$getx)
  expect_identical(parent.env(eval_env), environment())
  expect_identical(eval_env$self, A)
  expect_identical(eval_env$private, A$getprivate())

  # Eval environment for private methods should be the same
  expect_identical(eval_env, environment(A$getprivate()$getx_priv))

  # Behavioral tests
  expect_identical(A$x, 1)
  expect_null(A$y)
  expect_null(A$getx_foo)
  expect_identical(A$gety(), 2)  # Explicit access: private$y
  expect_identical(A$getx(), 1)  # Explicit access: self$x
  expect_identical(A$getx2(), 1) # Indirect access: private$getx_priv()
})


test_that("Private methods exist even when no private fields", {
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 1,
      getx = function() self$x,
      getx2 = function() private$getx_priv(),
      getself = function() self,
      getprivate = function() private
    ),
    private = list(
      getx_priv = function() self$x
    )
  )
  A <- AC$new()

  # The private binding environment contains private fields
  private_bind_env <- A$getprivate()
  expect_identical(ls(private_bind_env), "getx_priv")
  expect_identical(parent.env(private_bind_env), emptyenv())
})


test_that("Active bindings work", {
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 5
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(self$x * 2)
        else self$x <- value/2
      },

      sqrt_of_x = function(value) {
        if (!missing(value))
          # In "setter" role
          stop("Sorry this is a read-only variable.")
        else {
          # In "getter" role
          if (self$x < 0) stop("The requested value is not available.")
          else sqrt(self$x)
        }

      }
    )
  )
  A <- AC$new()

  expect_identical(A$x2, 10)
  A$x <- 20
  expect_identical(A$x2, 40)
  A$x2 <- 60
  expect_identical(A$x2, 60)
  expect_identical(A$x, 30)

  A$x <- -2
  expect_error(A$sqrt_of_x)
  # print does not throw an error trying to read
  # the active binding variables
  muted_print <- function(x) capture.output(print(x))
  expect_no_error(muted_print(A))
})


test_that("Locking works", {
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(x = 1, getx = function() self$x),
    private = list(y = 2, gety = function() self$y),
    lock_objects = TRUE
  )
  A <- AC$new()

  # Can modify fields
  expect_no_error(A$x <- 5)
  expect_identical(A$x, 5)
  expect_no_error(A$private$y <- 5)
  expect_identical(A$private$y, 5)

  # Can't modify methods
  expect_error(A$getx <- function() 1)
  expect_error(A$gety <- function() 2)

  # Can't add members
  expect_error(A$z <- 1)
  expect_error(A$private$z <- 1)


  # Not locked
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(x = 1, getx = function() x),
    private = list(y = 2, gety = function() y),
    lock_objects = FALSE
  )
  A <- AC$new()

  # Can modify fields
  expect_no_error(A$x <- 5)
  expect_identical(A$x, 5)
  expect_no_error(A$private$y <- 5)
  expect_identical(A$private$y, 5)

  # Can't modify methods
  expect_error(A$getx <- function() 1)
  expect_error(A$private$gety <- function() 2)

  # Can add members
  expect_no_error(A$z <- 1)
  expect_identical(A$z, 1)
  expect_no_error(A$private$z <- 1)
  expect_identical(A$private$z, 1)
})
