context("nonportable")

test_that("initialization", {
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      x = 1,
      initialize = function(x, y) {
        self$x <- getx() + x    # Assign to self; also access a method
        private$y <- y          # Assign to private
      },
      getx = function() x,
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
  AC <- R6Class("AC", portable = FALSE, public = list(x = 1))
  expect_error(AC$new(3))
})

test_that("empty members and methods are allowed", {
  # No initialize method: throw error if arguments are passed in
  AC <- R6Class("AC", portable = FALSE)
  expect_no_error(AC$new())
})


test_that("Private members are private, and self/private environments", {
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      x = 1,
      gety = function() private$y,
      gety2 = function() y,
      getx = function() self$x,
      getx2 = function() x,
      getx3 = function() getx_priv3(),
      getx4 = function() getx_priv4()
    ),
    private = list(
      y = 2,
      getx_priv3 = function() self$x,
      getx_priv4 = function() x
    )
  )
  A <- AC$new()

  # Environment structure
  expect_identical(A$self, A)
  expect_identical(A$private, parent.env(A))

  # Enclosing env for fublic and private methods is the public env
  expect_identical(A, environment(A$getx))
  expect_identical(A, environment(A$private$getx_priv3))

  # Behavioral tests
  expect_identical(A$x, 1)
  expect_null(A$y)
  expect_error(A$getx_priv3())
  expect_identical(A$gety(), 2)  # Explicit access: private$y
  expect_identical(A$gety2(), 2) # Implicit access: y
  expect_identical(A$getx(), 1)  # Explicit access: self$x
  expect_identical(A$getx2(), 1) # Implicit access: x
  expect_identical(A$getx3(), 1) # Call private method, which has explicit: self$x
  expect_identical(A$getx4(), 1) # Call private method, which has implicit: x
})


test_that("Active bindings work", {
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      x = 5
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(x * 2)
        else x <<- value/2
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
})


test_that("Locking objects", {
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(x = 1, getx = function() x),
    private = list(y = 2, gety = function() y),
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


test_that("Validity checks on creation", {
  fun <- function() 1  # Dummy function for tests

  # All arguments must be named
  expect_error(R6Class("AC", public = list(1)))
  expect_error(R6Class("AC", private = list(1)))
  expect_error(R6Class("AC", active = list(fun)))

  # Names can't be duplicated
  expect_error(R6Class("AC", public = list(a=1, a=2)))
  expect_error(R6Class("AC", public = list(a=1), private = list(a=1)))
  expect_error(R6Class("AC", private = list(a=1), active = list(a=fun)))

  # Reserved names
  expect_error(R6Class("AC", public = list(self = 1)))
  expect_error(R6Class("AC", private = list(private = 1)))
  expect_error(R6Class("AC", active = list(super = 1)))

  # `initialize` only allowed in public
  expect_error(R6Class("AC", private = list(initialize = fun)))
  expect_error(R6Class("AC", active = list(initialize = fun)))
})


test_that("default print method has a trailing newline", {

  ## This is kind of hackish, because both capture.output and
  ## expect_output drop the trailing newline. This function
  ## does not work in the general case, but it is good enough
  ## for this test.

  expect_output_n <- function(object) {
    tmp <- file()
    on.exit(close(tmp))
    sink(tmp)
    print(object)
    sink(NULL)
    output <- readChar(tmp, nchar = 10000)
    last_char <- substr(output, nchar(output), nchar(output))
    expect_that(last_char, equals("\n"))
  }

  AC <- R6Class("AC")
  expect_output_n(print(AC))

  A <- AC$new()
  expect_output_n(print(A))

  AC <- R6Class("AC", private = list( x = 2 ))
  expect_output_n(print(AC))

  A <- AC$new()
  expect_output_n(print(A))
})
