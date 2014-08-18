context("portable-inheritance")

test_that("Inheritance", {
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 0,
      z = 0,
      initialize = function(x) self$x <- x,
      getx = function() self$x,
      getx2 = function() self$x*2,
      getprivateA = function() private
    ),
    private = list(
      getz = function() self$z,
      getz2 = function() self$z*2
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(self$x * 2)
        else self$x <- value/2
      },
      x3 = function(value) {
        if (missing(value)) return(self$x * 3)
        else self$x <- value/3
      }
    )
  )
  BC <- R6Class("BC",
    portable = TRUE,
    inherit = AC,
    public = list(
      y = 0,
      z = 3,
      initialize = function(x, y) {
        super$initialize(x)
        self$y <- y
      },
      getx = function() self$x + 10,
      getprivateB = function() private
    ),
    private = list(
      getz = function() self$z + 10
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(self$x + 2)
        else self$x <- value-2
      }
    )
  )
  B <- BC$new(1, 2)

  # Environment checks
  eval_env <- environment(B$getx)
  super_bind_env <- eval_env$super
  super_eval_env <- environment(super_bind_env$getx)

  expect_identical(parent.env(super_bind_env), emptyenv())
  expect_identical(parent.env(super_eval_env), environment())
  expect_identical(super_eval_env$self, B)
  expect_identical(super_eval_env$private, B$getprivateA())
  expect_identical(B$getprivateA(), B$getprivateB())

  # Overridden public method
  expect_identical(eval_env, environment(B$getx))
  # Inherited public method
  environment(B$getx2)
  expect_identical(B, environment(B$getx2)$self)
  # Overridden private method
  expect_identical(eval_env, environment(B$getprivateA()$getz))
  # Inherited private method - should have same eval env as inherited public
  expect_identical(environment(B$getx2), environment(B$getprivateA()$getz2))

  # Behavioral tests
  # Overriding literals
  expect_identical(B$x, 1)
  expect_identical(B$y, 2)
  expect_identical(B$z, 3) # Subclass value overrides superclass value
  # Methods
  expect_identical(B$getx(), 11)                # Overridden public method
  expect_identical(B$getx2(), 2)                # Inherited public method
  expect_identical(B$getprivateA()$getz(), 13)  # Overriden private method
  expect_identical(B$getprivateA()$getz2(), 6)  # Inherited private method

  # Active bindings
  expect_identical(B$x2, 3) # Overridden
  expect_identical(B$x3, 3) # Inherited

  # Classes
  expect_identical(class(B), c("BC", "AC", "R6"))
})


test_that("Inheritance: superclass methods", {
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 0,
      initialize = function() {
        self$inc_x()
        private$inc_y()
        self$incz
      },
      inc_x = function() self$x <- self$x + 1,
      inc = function(val) val + 1,
      pinc = function(val) private$priv_inc(val), # Call private inc method
      gety = function() private$y,
      z = 0
    ),
    private = list(
      y = 0,
      inc_y = function() private$y <- private$y + 1,
      priv_inc = function(val) val + 1
    ),
    active = list(
      incz = function(value) {
        self$z <- z + 1
      }
    )
  )
  BC <- R6Class("BC",
    portable = TRUE,
    inherit = AC,
    public = list(
      inc_x = function() self$x <- self$x + 2,
      inc = function(val) super$inc(val) + 20
    ),
    private = list(
      inc_y = function() private$y <- private$y + 2,
      priv_inc = function(val) super$priv_inc(val) + 20
    ),
    active = list(
      incz = function(value) {
        self$z <- self$z + 2
      }
    )
  )
  B <- BC$new()

  # Testing overrides
  expect_identical(B$x, 2)       # Public
  expect_identical(B$gety(), 2)  # Private
  expect_identical(B$z, 2)       # Active
  # Calling superclass methods
  expect_identical(B$inc(0), 21)
  expect_identical(B$pinc(0), 21)


  # Multi-level inheritance
  CC <- R6Class("CC",
    portable = TRUE,
    inherit = BC,
    public = list(
      inc_x = function() self$x <- self$x + 3,
      inc = function(val) super$inc(val) + 300
    ),
    private = list(
      inc_y = function() private$y <- private$y + 3,
      priv_inc = function(val) super$priv_inc(val) + 300
    ),
    active = list(
      incz = function(value) {
        self$z <- self$z + 3
      }
    )
  )
  C <- CC$new()

  # Testing overrides
  expect_identical(C$x, 3)       # Public
  expect_identical(C$gety(), 3)  # Private
  expect_identical(C$z, 3)       # Active
  # Calling superclass methods (two levels)
  expect_identical(C$inc(0), 321)
  expect_identical(C$pinc(0), 321)

  # Classes
  expect_identical(class(C), c("CC", "BC", "AC", "R6"))
})


test_that("Inheritance: enclosing environments for super$ methods", {
  encA <- new.env()
  encB <- new.env()
  encC <- new.env()

  encA$n <- 1
  encB$n <- 20
  encC$n <- 300

  AC <- R6Class("AC",
    portable = TRUE,
    parent_env = encA,
    public = list(
      x = 0,
      initialize = function() {
        self$x <- self$get_n()
      },
      get_n = function() n,
      priv_get_n = function(val) private$get_n_priv()
    ),
    private = list(
      get_n_priv = function() n
    ),
    active = list(
      active_get_n = function() n
    )
  )
  A <- AC$new()
  expect_identical(A$x, 1)
  expect_identical(A$get_n(), 1)
  expect_identical(A$priv_get_n(), 1)
  expect_identical(A$active_get_n, 1)

  BC <- R6Class("BC",
    portable = TRUE,
    parent_env = encB,
    inherit = AC,
    public = list(
      x = 0,
      initialize = function() {
        super$initialize()
      },
      get_n = function() n + super$get_n(),
      priv_get_n = function(val) private$get_n_priv()
    ),
    private = list(
      get_n_priv = function() n + super$get_n_priv()
    ),
    active = list(
      active_get_n = function() n + super$active_get_n
    )
  )
  B <- BC$new()
  expect_identical(B$x, 21)
  expect_identical(B$get_n(), 21)
  expect_identical(B$priv_get_n(), 21)
  expect_identical(B$active_get_n, 21)

  CC <- R6Class("CC",
    portable = TRUE,
    parent_env = encC,
    inherit = BC,
    public = list(
      x = 0,
      initialize = function() {
        super$initialize()
      },
      get_n = function() n + super$get_n(),
      priv_get_n = function(val) private$get_n_priv()
    ),
    private = list(
      get_n_priv = function() n + super$get_n_priv()
    ),
    active = list(
      active_get_n = function() n + super$active_get_n
    )
  )
  C <- CC$new()
  expect_identical(C$x, 321)
  expect_identical(C$get_n(), 321)
  expect_identical(C$priv_get_n(), 321)
  expect_identical(C$active_get_n, 321)
})


test_that("Inheritance: enclosing environments for inherited methods", {
  encA <- new.env()
  encB <- new.env()
  encC <- new.env()

  encA$n <- 1
  encB$n <- 20
  encC$n <- 300

  AC <- R6Class("AC",
    portable = TRUE,
    parent_env = encA,
    public = list(
      get_n = function() n
    )
  )
  A <- AC$new()
  expect_identical(A$get_n(), 1)

  BC <- R6Class("BC",
    portable = TRUE,
    parent_env = encB,
    inherit = AC
  )
  B <- BC$new()
  # Since this inherits A's get_n() method, it should also inherit the
  # environment in which get_n() runs. This is necessary for inherited methods
  # to find methods from the correct namespace.
  expect_identical(B$get_n(), 1)

  CC <- R6Class("CC",
    portable = TRUE,
    parent_env = encC,
    inherit = BC,
    public = list(
      get_n = function() n + super$get_n()
    )
  )
  C <- CC$new()
  # When this calls super$get_n(), it should get B's version of get_n(), which
  # should in turn run in A's environment, returning 1. Add C's value of n, and
  # the total is 301.
  expect_identical(C$get_n(), 301)
})


test_that("Inheritance hierarchy for super$ methods", {
  AC <- R6Class("AC", portable = TRUE,
    public = list(n = function() 0 + 1)
  )
  expect_identical(AC$new()$n(), 1)

  BC <- R6Class("BC", portable = TRUE,
    public = list(n = function() super$n() + 10),
    inherit = AC
  )
  expect_identical(BC$new()$n(), 11)

  CC <- R6Class("CC", portable = TRUE,
    inherit = BC
  )
  # This should equal 11 because it inherits BC's n(), which adds 1 to AC's n()
  expect_identical(CC$new()$n(), 11)

  # Skipping one level of inheritance ---------------------------------
  AC <- R6Class("AC", portable = TRUE,
    public = list(n = function() 0 + 1)
  )
  expect_identical(AC$new()$n(), 1)

  BC <- R6Class("BC", portable = TRUE,
    inherit = AC
  )
  expect_identical(BC$new()$n(), 1)

  CC <- R6Class("CC", portable = TRUE,
    public = list(n = function() super$n() + 100),
    inherit = BC
  )
  # This should equal 101 because BC inherits AC's n()
  expect_identical(CC$new()$n(), 101)

  DC <- R6Class("DC", portable = TRUE,
    inherit = CC
  )
  # This should equal 101 because DC inherits CC's n(), and BC inherits AC's n()
  expect_identical(DC$new()$n(), 101)

  # Skipping two level of inheritance ---------------------------------
  AC <- R6Class("AC", portable = TRUE,
    public = list(n = function() 0 + 1)
  )
  expect_identical(AC$new()$n(), 1)

  BC <- R6Class("BC", portable = TRUE, inherit = AC)
  expect_identical(BC$new()$n(), 1)

  CC <- R6Class("CC", portable = TRUE, inherit = BC)
  expect_identical(CC$new()$n(), 1)
})


test_that("sub and superclass must both be portable or non-portable", {
  AC <- R6Class("AC", portable = FALSE, public = list(x=1))
  BC <- R6Class("BC", portable = TRUE, inherit = AC)
  expect_error(BC$new())

  AC <- R6Class("AC", portable = TRUE, public = list(x=1))
  BC <- R6Class("BC", portable = FALSE, inherit = AC)
  expect_error(BC$new())
})


test_that("Inheritance is dynamic", {
  AC <- R6Class("AC",
    public = list(x = 1, initialize = function() self$x <<- self$x + 10)
  )
  BC <- R6Class("BC", inherit = AC)
  expect_identical(BC$new()$x, 11)

  AC <- R6Class("AC",
    public = list(x = 2, initialize = function() self$x <<- self$x + 20)
  )
  expect_identical(BC$new()$x, 22)

  # BC doesn't contain AC, and it has less stuff in it, so it should be smaller
  # than AC.
  expect_true(pryr::object_size(BC) < pryr::object_size(AC))
})


test_that("Private env is created when all private members are inherited", {
  # Private contains fields only
  AC <- R6Class("AC",
    public = list(getx = function() private$x),
    private = list(x = 1)
  )
  BC <- R6Class("BC", inherit = AC)
  expect_identical(BC$new()$getx(), 1)


  # Private contains functions only
  AC <- R6Class("AC",
    public = list(getx = function() private$x()),
    private = list(x = function() 1)
  )
  BC <- R6Class("BC", inherit = AC)
  expect_identical(BC$new()$getx(), 1)
})
