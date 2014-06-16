context("refclass")

test_that("initialization", {
  AC <- createRefClass("AC",
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
  fun <- function() 1  # Dummy function for tests

  # All arguments must be named
  expect_error(createRefClass("AC", public = list(1)))
  expect_error(createRefClass("AC", private = list(1)))
  expect_error(createRefClass("AC", active = list(fun)))

  # Names can't be duplicated
  expect_error(createRefClass("AC", public = list(a=1, a=2)))
  expect_error(createRefClass("AC", public = list(a=1), private = list(a=1)))
  expect_error(createRefClass("AC", private = list(a=1), active = list(a=fun)))

  # Reserved names
  expect_error(createRefClass("AC", public = list(self = 1)))
  expect_error(createRefClass("AC", private = list(private = 1)))
  expect_error(createRefClass("AC", active = list(super = 1)))
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


test_that("Inheritance: superclass methods", {
  AC <- createRefClass("AC",
    public = list(
      x = 0,
      initialize = function() {
        inc_x()
        inc_self_x()
        inc_y()
        inc_self_y()
        incz
      },
      inc_x = function() x <<- x + 1,
      inc_self_x = function() self$x <- self$x + 10,
      inc = function(val) val + 1,
      pinc = function(val) priv_inc(val), # Call private inc method
      z = 0
    ),
    private = list(
      y = 0,
      inc_y = function() y <<- y + 1,
      inc_self_y = function() private$y <- private$y + 10,
      priv_inc = function(val) val + 1
    ),
    active = list(
      incz = function(value) {
        z <<- z + 1
      }
    )
  )
  BC <- createRefClass("BC",
    inherit = AC,
    public = list(
      inc_x = function() x <<- x + 2,
      inc_self_x = function() self$x <- self$x + 20,
      inc = function(val) super$inc(val) + 20
    ),
    private = list(
      inc_y = function() y <<- y + 2,
      inc_self_y = function() private$y <- private$y + 20,
      priv_inc = function(val) super$priv_inc(val) + 20
    ),
    active = list(
      incz = function(value) {
        z <<- z + 2
      }
    )
  )
  B <- BC$new()

  # Environment checks
  expect_identical(parent.env(B$super), emptyenv())
  # Enclosing env for functions in $super is a child of $self
  expect_identical(parent.env(environment(B$super$inc_x)), B)

  # Testing overrides
  expect_identical(B$x, 22)          # Public
  expect_identical(B$private$y, 22)  # Private
  expect_identical(B$z, 2)           # Active
  # Calling superclass methods
  expect_identical(B$inc(0), 21)
  expect_identical(B$pinc(0), 21)


  # Multi-level inheritance
  CC <- createRefClass("CC",
    inherit = BC,
    public = list(
      inc_x = function() x <<- x + 3,
      inc_self_x = function() self$x <- self$x + 30,
      inc = function(val) super$inc(val) + 300
    ),
    private = list(
      inc_y = function() y <<- y + 3,
      inc_self_y = function() private$y <- private$y + 30,
      priv_inc = function(val) super$priv_inc(val) + 300
    ),
    active = list(
      incz = function(value) {
        z <<- z + 3
      }
    )
  )
  C <- CC$new()

  # Testing overrides
  expect_identical(C$x, 33)          # Public
  expect_identical(C$private$y, 33)  # Private
  expect_identical(C$z, 3)           # Active
  # Calling superclass methods (two levels)
  expect_identical(C$inc(0), 321)
  expect_identical(C$pinc(0), 321)

  # Classes
  expect_identical(class(C), c("CC", "BC", "AC", "RefClass"))})
