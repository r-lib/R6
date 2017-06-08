context("clone")

test_that("Can't use reserved name 'clone'", {
  expect_error(R6Class("AC", public = list(clone = function() NULL)))
  expect_error(R6Class("AC", private = list(clone = function() NULL)))
  expect_error(R6Class("AC", active = list(clone = function() NULL)))
})


test_that("Can disable cloning", {
  AC <- R6Class("AC", public = list(x = 1), cloneable = FALSE)
  a <- AC$new()
  expect_null(a$clone)
})


test_that("Cloning portable objects with public only", {
  parenv <- new.env()
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 1,
      getx = function() self$x
    ),
    parent_env = parenv
  )

  # Behavioral tests
  a <- AC$new()
  b <- a$clone()
  b$x <- 2
  expect_identical(a$getx(), 1)
  expect_identical(b$getx(), 2)

  # Enclosing environment for methods
  a_enclos_env <- environment(a$getx)
  b_enclos_env <- environment(b$getx)

  # self points to the object (public binding env)
  expect_identical(a_enclos_env$self, a)
  expect_identical(b_enclos_env$self, b)

  # Parent of enclosing env should be class's parent_env
  expect_identical(parent.env(a_enclos_env), parenv)
  expect_identical(parent.env(b_enclos_env), parenv)

  # Enclosing env only contains self
  expect_identical(ls(a_enclos_env), "self")
  expect_identical(ls(b_enclos_env), "self")

  # Parent of binding env is emptyenv(), for portable classes
  expect_identical(parent.env(a), emptyenv())
  expect_identical(parent.env(b), emptyenv())

  # Cloning a clone
  c <- b$clone()
  expect_identical(c$getx(), 2)
  c$x <- 3
  expect_identical(c$getx(), 3)
})


test_that("Cloning non-portable objects with public only", {
  parenv <- new.env()
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      x = 1,
      getx = function() self$x
    ),
    parent_env = parenv
  )

  # Behavioral tests
  a <- AC$new()
  b <- a$clone()
  b$x <- 2
  expect_identical(a$getx(), 1)
  expect_identical(b$getx(), 2)

  # Enclosing environment for methods
  a_enclos_env <- environment(a$getx)
  b_enclos_env <- environment(b$getx)

  # Enclosing env is identical to public binding env
  expect_identical(a_enclos_env, a)
  expect_identical(b_enclos_env, b)

  # self points back to the object (public binding env)
  expect_identical(a$self, a)
  expect_identical(b$self, b)

  # Parent of enclosing env should be class's parent_env
  expect_identical(parent.env(a_enclos_env), parenv)
  expect_identical(parent.env(b_enclos_env), parenv)

  # Contains correct objects
  expect_identical(ls(a), c("clone", "getx", "self", "x"))
  expect_identical(ls(b), c("clone", "getx", "self", "x"))
})


test_that("Cloning portable objects with public and private", {
  parenv <- new.env()
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 1,
      getx = function() self$x,
      getprivate = function() private,
      sety = function(value) private$y <- value
    ),
    private = list(
      y = 1,
      gety = function() private$y
    ),
    parent_env = parenv
  )

  # Behavioral tests
  a <- AC$new()
  b <- a$clone()
  b$x <- 2
  b$sety(2)
  expect_identical(a$getx(), 1)
  expect_identical(a$getprivate()$gety(), 1)
  expect_identical(b$getx(), 2)
  expect_identical(b$getprivate()$gety(), 2)

  # Enclosing environment for methods
  a_enclos_env <- environment(a$getx)
  b_enclos_env <- environment(b$getx)
  # Enclosing environment for private methods is same
  expect_identical(a_enclos_env, environment(a$getprivate()$gety))
  expect_identical(b_enclos_env, environment(b$getprivate()$gety))

  # self points to the object (public binding env)
  expect_identical(a_enclos_env$self, a)
  expect_identical(b_enclos_env$self, b)

  # Parent of enclosing env should be class's parent_env
  expect_identical(parent.env(a_enclos_env), parenv)
  expect_identical(parent.env(b_enclos_env), parenv)
  # Parent of public binding env is emptyenv(), for portable classes
  expect_identical(parent.env(a), emptyenv())
  expect_identical(parent.env(b), emptyenv())
  # Parent of private binding env is emptyenv(), for portable classes
  expect_identical(parent.env(a$getprivate()), emptyenv())
  expect_identical(parent.env(b$getprivate()), emptyenv())

  # Enclosing env only contains self and private
  expect_identical(ls(a_enclos_env), c("private", "self"))
  expect_identical(ls(b_enclos_env), c("private", "self"))
  # public binding env contains just the public members
  expect_identical(ls(a), c("clone", "getprivate", "getx", "sety", "x"))
  expect_identical(ls(b), c("clone", "getprivate", "getx", "sety", "x"))
  # private binding env contains just the private members
  expect_identical(ls(a$getprivate()), c("gety", "y"))
  expect_identical(ls(b$getprivate()), c("gety", "y"))
})


test_that("Cloning non-portable objects with public and private", {
  parenv <- new.env()
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      x = 1,
      getx = function() self$x,
      getprivate = function() private,
      sety = function(value) private$y <- value
    ),
    private = list(
      y = 1,
      gety = function() private$y
    ),
    parent_env = parenv
  )

  # Behavioral tests
  a <- AC$new()
  b <- a$clone()
  b$x <- 2
  b$sety(2)
  expect_identical(a$getx(), 1)
  expect_identical(a$getprivate()$gety(), 1)
  expect_identical(b$getx(), 2)
  expect_identical(b$getprivate()$gety(), 2)

  # Enclosing environment for methods
  a_enclos_env <- environment(a$getx)
  b_enclos_env <- environment(b$getx)
  # Enclosing env is identical to public binding env
  expect_identical(a_enclos_env, a)
  expect_identical(b_enclos_env, b)
  # Enclosing environment for private methods is same
  expect_identical(a_enclos_env, environment(a$getprivate()$gety))
  expect_identical(b_enclos_env, environment(b$getprivate()$gety))

  # self points to the object (public binding env)
  expect_identical(a_enclos_env$self, a)
  expect_identical(b_enclos_env$self, b)

  # Parent of enclosing env should be private env
  expect_identical(parent.env(a), a$getprivate())
  expect_identical(parent.env(b), b$getprivate())
  # Parent of private env should be class's parent_env
  expect_identical(parent.env(a$getprivate()), parenv)
  expect_identical(parent.env(b$getprivate()), parenv)

  # Public binding env (AKA enclosing env) contains self, private, and members
  expect_identical(ls(a),
    c("clone", "getprivate", "getx", "private", "self", "sety", "x"))
  expect_identical(ls(b),
    c("clone", "getprivate", "getx", "private", "self", "sety", "x"))
  # private binding env contains just the private members
  expect_identical(ls(a$getprivate()), c("gety", "y"))
  expect_identical(ls(b$getprivate()), c("gety", "y"))
})


test_that("Cloning subclasses with inherited private fields", {
  # For issue #72
  AC <- R6Class("AC",
    public = list(
      getx = function() private$x
    ),
    private = list(
      x = 1
    )
  )

  BC <- R6Class("BC",
    inherit = AC,
    public = list(
      getx = function() super$getx()
    )
  )

  b1 <- BC$new()
  b2 <- b1$clone()
  expect_identical(b1$getx(), 1)
  expect_identical(b2$getx(), 1)
})


test_that("Cloning active bindings", {
  AC <- R6Class("AC",
    public = list(
      x = 1
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) self$x * 2
        else self$x <- value / 2
      }
    )
  )

  a <- AC$new()
  b <- a$clone()

  a$x <- 10
  expect_identical(a$x2, 20)
  a$x2 <- 22
  expect_identical(a$x, 11)

  expect_identical(b$x2, 2)
  b$x <- 2
  expect_identical(b$x2, 4)
  b$x2 <- 10
  expect_identical(b$x, 5)
})


test_that("Cloning active binding in superclass", {
  AC <- R6Class("AC",
    public = list(
      x = 1
    ),
    active = list(
      x2 = function(value){
        if (missing(value)) self$x * 2
        else self$x <- value / 2
      }
    )
  )

  BC <- R6Class("BC",
    inherit = AC,
    active = list(
      x2 = function(value){
        if (missing(value)) super$x2 * 2
        else super$x2 <- value / 2
      }
    )
  )

  a <- AC$new()
  a$x <- 10
  expect_identical(a$x2, 20)
  a$x2 <- 22
  expect_identical(a$x, 11)

  b <- BC$new()
  b$x <- 10
  expect_identical(b$x2, 40)
  b$x <- 11
  expect_identical(b$x2, 44)

  b1 <- b$clone()
  expect_identical(b1$x2, 44)
  b1$x <- 12
  expect_identical(b1$x2, 48)
})


test_that("Cloning active binding in two levels of inheritance", {
  # For issue #119
  A <- R6Class("A",
    public = list(
      methodA = function() "A"
    ),
    active = list(
      x = function() "x"
    )
  )

  B <- R6Class("B",
    inherit = A,
    public = list(
      methodB = function() {
        super$methodA()
      }
    )
  )

  C <- R6Class("C",
    inherit = B,
    public = list(
      methodC = function() {
        super$methodB()
      }
    )
  )

  C1 <- C$new()
  C2 <- C1$clone()
  expect_identical(C2$methodC(), "A")
  expect_identical(
    C1$.__enclos_env__$super$.__enclos_env__,
    environment(C1$.__enclos_env__$super$methodB)
  )
})


test_that("Lock state", {
  AC <- R6Class("AC",
    public = list(
      x = 1,
      yval = function(y) {
        if (missing(y)) private$y
        else private$y <- y
      }
    ),
    private = list(w = 1),
    lock_objects = TRUE
  )

  a <- AC$new()
  b <- a$clone()
  expect_error(a$z <- 1)
  expect_error(b$z <- 1)

  expect_identical(a$yval(), NULL)
  expect_identical(b$yval(), NULL)
  expect_error(a$yval(1))
  expect_error(b$yval(1))

  # With lock = FALSE
  AC <- R6Class("AC",
    public = list(
      x = 1,
      yval = function(y) {
        if (missing(y)) private$y
        else private$y <- y
      }
    ),
    private = list(w = 1),
    lock_objects = FALSE
  )

  a <- AC$new()
  b <- a$clone()
  a$y <- 1
  b$y <- 1
  expect_identical(a$y, 1)
  expect_identical(b$y, 1)

  expect_identical(a$yval(), NULL)
  expect_identical(b$yval(), NULL)
  a$yval(1)
  b$yval(1)
  expect_identical(a$yval(), 1)
  expect_identical(b$yval(), 1)
})


test_that("Cloning inherited methods", {
  C1 <- R6Class("C1",
    public = list(
      x = 1,
      getx = function() self$x,
      addx = function() self$x + 10
    ),
    active = list(
      xa = function(val) {
        if (missing(val)) self$x * 2
        else self$x <- val / 2
      }
    )
  )

  C2 <- R6Class("C2",
    inherit = C1,
    public = list(
      x = 2,
      addx = function() super$addx() + 10
    ),
    active = list(
      xa = function(val) {
        if (missing(val)) self$x * 3
        else self$x <- val / 3
      }
    )
  )

  a <- C2$new()
  b <- a$clone()

  expect_identical(b$getx(), 2)
  expect_identical(b$addx(), 22)
  b$x <- 3
  expect_identical(b$getx(), 3)
  expect_identical(b$addx(), 23)

  expect_identical(b$xa, 9)
  b$xa <- 12
  expect_identical(b$x, 4)

  # Make sure a was unaffected
  expect_identical(a$x, 2)


  # Same as previous, but with another copy and another level of inheritance
  C3 <- R6Class("C3",
    inherit = C2,
    public = list(
      x = 3,
      addx = function() super$addx() + 20
    ),
    active = list(
      xa = function(val) {
        if (missing(val)) self$x * 4
        else self$x <- val / 4
      }
    )
  )

  a <- C3$new()
  b <- a$clone()
  c <- b$clone()
  b$x <- 4
  c$x <- 5
  expect_identical(a$getx(), 3)
  expect_identical(a$addx(), 43)
  expect_identical(b$getx(), 4)
  expect_identical(b$addx(), 44)
  expect_identical(c$getx(), 5)
  expect_identical(c$addx(), 45)

  expect_identical(c$xa, 20)
  c$xa <- 24
  expect_identical(c$x, 6)

  # Make sure a and b were unaffected
  expect_identical(a$x, 3)
  expect_identical(b$x, 4)


  # Three levels; don't override active binding
  C3na <- R6Class("C3na",
    inherit = C2,
    public = list(x = 3)
  )
  a <- C3na$new()
  b <- a$clone()
  b$x <- 4
  expect_identical(b$xa, 12)
  b$xa <- 15
  expect_identical(b$x, 5)
})


test_that("Deep cloning", {
  AC <- R6Class("AC", public = list(x = 1))
  BC <- R6Class("BC",
    public = list(
      x = NULL,
      y = function() private$y_,
      initialize = function() {
        self$x <- AC$new()
        private$y_ <- AC$new()
      }
    ),
    private = list(
      y_ = NULL
    )
  )

  b <- BC$new()
  b2 <- b$clone(deep = FALSE)
  expect_identical(b$x, b2$x)
  expect_identical(b$y(), b2$y())

  b <- BC$new()
  b2 <- b$clone(deep = TRUE)
  expect_false(identical(b$x, b2$x))
  expect_false(identical(b$y(), b2$y()))
  # Make sure b2$x and b2$y are properly cloned R6 objects
  expect_identical(class(b2$x), c("AC", "R6"))
  expect_identical(class(b2$y()), c("AC", "R6"))


  # Deep cloning with multiple levels
  CC <- R6Class("CC",
    public = list(
      x = NULL,
      initialize = function() {
        self$x <- BC$new()
      }
    )
  )

  c <- CC$new()
  c2 <- c$clone(deep = TRUE)
  expect_false(identical(c$x, c2$x))
  expect_false(identical(c$x$x, c2$x$x))
  # Make sure c2$x and c2$x$x are properly cloned R6 objects
  expect_identical(class(c2$x), c("BC", "R6"))
  expect_identical(class(c2$x$x), c("AC", "R6"))


  # Deep cloning with custom function
  AC <- R6Class("AC", public = list(x = 1))
  BC <- R6Class("BC",
    public = list(
      x = "AC",
      y = "AC",
      z = "AC",
      initialize = function() {
        self$x <- AC$new()
        self$y <- AC$new()
        self$z <- AC$new()
      }
    ),
    private = list(
      deep_clone = function(name, val) {
        if (name %in% c("x", "y"))
          val$clone()
        else
          val
      }
    )
  )
  a <- BC$new()
  b <- a$clone()
  c <- a$clone(deep = TRUE)

  a$x$x <- 2
  a$y$x <- 3
  a$z$x <- 4

  # b is shallow clone
  expect_identical(a$x$x, b$x$x)
  expect_identical(a$y$x, b$y$x)
  expect_identical(a$z$x, b$z$x)

  # c has deep clones of x and y, but not z
  expect_identical(c$x$x, 1)
  expect_identical(c$y$x, 1)
  expect_identical(a$z$x, c$z$x)
})


test_that("Deep cloning non-portable classes", {
  # Make sure deep cloning doesn't lead to infinite loop because of `self`
  AC <- R6Class("AC", portable = FALSE, public = list(x = 1))
  a <- AC$new()
  a$x <- 2
  a2 <- a$clone(deep = TRUE)

  expect_identical(a2$x, 2)
  expect_identical(a2$self, a2)
})
