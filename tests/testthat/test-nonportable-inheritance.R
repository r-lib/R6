context("nonportable-inheritance")

test_that("Inheritance", {
  AC <- R6Class("AC",
    portable = FALSE,
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
  BC <- R6Class("BC",
    portable = FALSE,
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
  expect_identical(B, environment(B$getx))                      # Overridden public method
  expect_identical(B, parent.env(environment(B$getx2)))         # Inherited public method
  expect_identical(B, environment(B$private$getz))              # Overridden private method
  expect_identical(B, parent.env(environment(B$private$getz2))) # Inherited private method

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
  expect_identical(class(B), c("BC", "AC", "R6"))
})


test_that("Inheritance: superclass methods", {
  AC <- R6Class("AC",
    portable = FALSE,
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
  BC <- R6Class("BC",
    portable = FALSE,
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
  CC <- R6Class("CC",
    portable = FALSE,
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
  expect_identical(class(C), c("CC", "BC", "AC", "R6"))
})


test_that("Inheritance hierarchy for super$ methods", {
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(n = function() 0 + 1)
  )
  expect_identical(AC$new()$n(), 1)

  BC <- R6Class("BC",
    portable = FALSE,
    public = list(n = function() super$n() + 10),
    inherit = AC
  )
  expect_identical(BC$new()$n(), 11)

  CC <- R6Class("CC",
    portable = FALSE,
    inherit = BC
  )
  # This should equal 11 because it inherits BC's n(), which adds 1 to AC's n()
  expect_identical(CC$new()$n(), 11)

  # Skipping one level of inheritance ---------------------------------
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(n = function() 0 + 1)
  )
  expect_identical(AC$new()$n(), 1)

  BC <- R6Class("BC",
    portable = FALSE,
    inherit = AC
  )
  expect_identical(BC$new()$n(), 1)

  CC <- R6Class("CC",
    portable = FALSE,
    public = list(n = function() super$n() + 100),
    inherit = BC
  )
  # This should equal 101 because BC inherits AC's n()
  expect_identical(CC$new()$n(), 101)

  DC <- R6Class("DC",
    portable = FALSE,
    inherit = CC
  )
  # This should equal 101 because DC inherits CC's n(), and BC inherits AC's n()
  expect_identical(DC$new()$n(), 101)

  # Skipping two level of inheritance ---------------------------------
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(n = function() 0 + 1)
  )
  expect_identical(AC$new()$n(), 1)

  BC <- R6Class("BC", portable = FALSE, inherit = AC)
  expect_identical(BC$new()$n(), 1)

  CC <- R6Class("CC", portable = FALSE, inherit = BC)
  expect_identical(CC$new()$n(), 1)
})


test_that("Private env is created when all private members are inherited", {
  # Private contains fields only
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      getx = function() x,
      getx2 = function() private$x
    ),
    private = list(x = 1)
  )
  BC <- R6Class("BC", portable = FALSE, inherit = AC)
  expect_identical(BC$new()$getx(), 1)
  expect_identical(BC$new()$getx2(), 1)

  # Private contains functions only
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      getx = function() x(),
      getx2 = function() private$x()
    ),
    private = list(x = function() 1)
  )
  BC <- R6Class("BC", portable = FALSE, inherit = AC)
  expect_identical(BC$new()$getx(), 1)
  expect_identical(BC$new()$getx2(), 1)
})
