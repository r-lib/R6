context("refclass")

test_that("initialization", {
  AC <- createRefClass("AC",
    public = list(
      x = 1,
      initialize = function(x, y) {
        self$x <- x
        private$y <- y
      },
      gety = function() private$y
    ),
    private = list(
      y = 2
    )
  )

  A <- AC$new(2, 3)

  expect_identical(A$x, 2)
  expect_identical(A$gety(), 3)
})


test_that("Private members are private, and self/private environments", {
  AC <- createRefClass("AC",
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
  AC <- createRefClass("AC",
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


test_that("Locking works", {
  AC <- createRefClass("AC",
    public = list(x = 1),
    private = list(y = 2),
    lock = TRUE
  )
  A <- AC$new()

  expect_that(A$x <- 5, not(throws_error()))
  expect_identical(A$x, 5)
  expect_error(A$z <- 1)
  expect_error(A$private$z <- 1)

  # Not locked
  AC <- createRefClass("AC",
    public = list(x = 1),
    private = list(y = 2),
    lock = FALSE
  )
  A <- AC$new()

  expect_that(A$x <- 5, not(throws_error()))
  expect_identical(A$x, 5)
  expect_that(A$z <- 1, not(throws_error()))
  expect_that(A$private$w <- 1, not(throws_error()))
})


test_that("Validity checks on creation", {
  # All arguments must be named
  expect_error(createRefClass("AC", public = list(1)))
  expect_error(createRefClass("AC", private = list(1)))
  expect_error(createRefClass("AC", active = list(1)))

  # Names can't be duplicated
  expect_error(createRefClass("AC", public = list(a=1, a=2)))
  expect_error(createRefClass("AC", public = list(a=1), private = list(a=1)))
  expect_error(createRefClass("AC", private = list(a=1), active = list(a=1)))
})


test_that("Inheritance", {
  AC <- createRefClass("AC",
    public = list(
      x = 0,
      z = 0,
      initialize = function(x) self$x <- x,
      getx = function() x,
      getx2 = function() x*2
    ),
    private = list(
      getz = function() z,
      getz2 = function() z*2
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(x * 2)
        else x <<- value/2
      },
      x3 = function(value) {
        if (missing(value)) return(x * 3)
        else x <<- value/3
      }
    )
  )
  BC <- createRefClass("BC",
    inherit = AC,
    public = list(
      y = 0,
      z = 3,
      initialize = function(x, y) {
        super$initialize(x)
        self$y <- y
      },
      getx = function() x + 10
    ),
    private = list(
      getz = function() z + 10
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(x + 2)
        else x <<- value-2
      }
    )
  )
  B <- BC$new(1, 2)

  # Environment checks
  expect_identical(B, environment(B$getx))          # Overridden public method
  expect_identical(B, environment(B$getx2))         # Inherited public method
  expect_identical(B, environment(B$private$getz))  # Overridden private method
  expect_identical(B, environment(B$private$getz2)) # Inherited private method

  # Behavioral tests
  # Overriding literals
  expect_identical(B$x, 1)
  expect_identical(B$y, 2)
  expect_identical(B$z, 3) # Subclass value overrides superclass value
  # Methods
  expect_identical(B$getx(), 11)          # Overridden public method
  expect_identical(B$getx2(), 2)          # Inherited public method
  expect_identical(B$private$getz(), 13)  # Overriden private method
  expect_identical(B$private$getz2(), 6)  # Inherited private method

  # Active bindings
  expect_identical(B$x2, 3) # Overridden
  expect_identical(B$x3, 3) # Inherited

  # Classes
  expect_identical(class(B), c("BC", "AC", "RefClass"))
})


