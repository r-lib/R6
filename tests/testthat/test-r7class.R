context("R7")

test_that("initialization", {
  AC <- R7Class("AC",
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
  AC <- R7Class("AC", public = list(x = 1))
  expect_error(AC$new(3))
})

test_that("empty members and methods are allowed", {
  # No initialize method: throw error if arguments are passed in
  AC <- R7Class("AC")
  expect_that(AC$new(), not(throws_error()))
})


test_that("Private members are private, and self/private environments", {
  AC <- R7Class("AC",
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
  AC <- R7Class("AC",
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
  AC <- R7Class("AC",
    public = list(
      x = 5
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(self$x * 2)
        else self$x <- value/2
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
  AC <- R7Class("AC",
    public = list(x = 1),
    private = list(y = 2),
    lock = TRUE
  )
  A <- AC$new()

  expect_that(A$x <- 5, not(throws_error()))
  expect_identical(A$x, 5)
  expect_error(A$z <- 1)

  # Not locked
  AC <- R7Class("AC",
    public = list(x = 1),
    private = list(y = 2),
    lock = FALSE
  )
  A <- AC$new()

  expect_that(A$x <- 5, not(throws_error()))
  expect_identical(A$x, 5)
  expect_that(A$z <- 1, not(throws_error()))
})


test_that("Validity checks on creation", {
  fun <- function() 1  # Dummy function for tests

  # All arguments must be named
  expect_error(R7Class("AC", public = list(1)))
  expect_error(R7Class("AC", private = list(1)))
  expect_error(R7Class("AC", active = list(fun)))

  # Names can't be duplicated
  expect_error(R7Class("AC", public = list(a=1, a=2)))
  expect_error(R7Class("AC", public = list(a=1), private = list(a=1)))
  expect_error(R7Class("AC", private = list(a=1), active = list(a=fun)))

  # Reserved names
  expect_error(R7Class("AC", public = list(self = 1)))
  expect_error(R7Class("AC", private = list(private = 1)))
  expect_error(R7Class("AC", active = list(super = 1)))

  # `initialize` only allowed in public
  expect_error(R7Class("AC", private = list(initialize = fun)))
  expect_error(R7Class("AC", active = list(initialize = fun)))
})


test_that("Inheritance", {
  AC <- R7Class("AC",
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
  BC <- R7Class("BC",
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

  expect_identical(eval_env, environment(B$getx))                # Overridden public method
  expect_identical(eval_env, environment(B$getx2))               # Inherited public method
  expect_identical(eval_env, environment(B$getprivateA()$getz))  # Overridden private method
  expect_identical(eval_env, environment(B$getprivateA()$getz2)) # Inherited private method

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
  expect_identical(class(B), c("BC", "AC", "R7"))
})


test_that("Inheritance: superclass methods", {
  AC <- R7Class("AC",
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
  BC <- R7Class("BC",
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
  CC <- R7Class("CC",
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
  expect_identical(class(C), c("CC", "BC", "AC", "R7"))
})
