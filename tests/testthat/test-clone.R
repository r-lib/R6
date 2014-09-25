context("clone")

test_that("Cloning portable objects with public only", {
  parenv <- new.env()
  AC <- R6Class("AC",
    portable = TRUE,
    public = list(
      x = 1,
      getx = function() self$x,
      clone = function() R6::clone()
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
})


test_that("Cloning non-portable objects with public only", {
  parenv <- new.env()
  AC <- R6Class("AC",
    portable = FALSE,
    public = list(
      x = 1,
      getx = function() self$x,
      clone = function() R6::clone()
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
      sety = function(value) private$y <- value,
      clone = function() R6::clone()
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
      sety = function(value) private$y <- value,
      clone = function() R6::clone()
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

