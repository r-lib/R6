context("initialize")

test_that("new() args with no initialize()", {
  NoInitializer <- R6Class("NoInitializer")
  expect_identical(formals(NoInitializer$new), NULL)
})

test_that("new() args with empty initialize()", {
  EmptyInitializer <- R6Class("EmptyInitializer", public = list(initialize = function() NULL))
  expect_null(formals(EmptyInitializer$new))
})

test_that("new() args with initialize() with args", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = )))
})

test_that("new() args with initialize() with args and defaults", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a = "", b) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = , b = )))
})

test_that("new() args with initialize() with dots", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a = "", ..., b) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = , ... = , b = )))
})

test_that("new() args with inherited initialize() with args", {
  A <- R6Class("A", public = list(initialize = function(a = "", ..., b) NULL))
  B <- R6Class("B", inherit = A)
  expect_identical(formals(B$new), as.pairlist(alist(... = )))
})

test_that("new() args with inherited class without initialize()", {
  # For a subclass without its own initialize() method, the $new() method must
  # take `...` because the superclass might have an initializer -- the
  # inheritance is resolved when $new() is called, and not before (see #12 for
  # reasons why), so the $new() method can't know whether the superclass has an
  # initializer, or what its parameters are.
  A <- R6Class("A")
  B <- R6Class("B", inherit = A)
  expect_identical(formals(B$new), as.pairlist(alist(... = )))
})


test_that("Missingness is passed along to initialize()", {
  Missing <- R6Class("Missing",
    public = list(
      initialize = function(a, b = 1, ...) {
        self$missing_a <- missing(a)
        self$missing_b <- missing(b)

        self$b <- b
      },
      missing_a = NULL,
      missing_b = NULL,
      b = NULL
    )
  )
  x <- Missing$new()
  expect_true(x$missing_a)
  expect_true(x$missing_b)

  MissingSubclass <- R6Class("MissingSubclass", inherit = Missing)
  x <- MissingSubclass$new()
  expect_true(x$missing_a)
  expect_true(x$missing_b)
})


test_that("Initialize() evaluates default values in the correct scope", {
  # Initializers should evaluate arguments in the scope of the new object, so
  # that self$ and private$ can be used as default values, and so that variables
  # from environment where the class was defined can be accessed.
  x_init <- 1
  A <- R6Class("A", public = list(
    initialize = function(x = x_init, y = self$y_init) {
      self$x <- x
      self$y <- y
    },
    x = NULL,
    y_init = 2,
    y = 1
  ))

  a <- A$new()
  expect_identical(a$x, 1)
  expect_identical(a$y, 2)

  a <- A$new(10)
  expect_identical(a$x, 10)
  expect_identical(a$y, 2)

  a <- A$new(, 10)
  expect_identical(a$x, 1)
  expect_identical(a$y, 10)

  a <- A$new(y = 10)
  expect_identical(a$x, 1)
  expect_identical(a$y, 10)


  A <- R6Class("A", public = list(
    initialize = function(x = x_init, y = self$y_init, ..., z) {
      self$x <- x
      self$y <- y
      self$dots <- list(...)
      self$z <- if (missing(z)) 3 else z
    },
    x = NULL,
    y_init = 2,
    y = 1,
    z = NULL,
    dots = list()
  ))

  a <- A$new()
  expect_identical(a$dots, list())
  expect_identical(a$x, 1)
  expect_identical(a$y, 2)
  expect_identical(a$z, 3)

  a <-A$new(, 20, 100)
  expect_identical(a$dots, list(100))
  expect_identical(a$x, 1)
  expect_identical(a$y, 20)
  expect_identical(a$z, 3)

  a <-A$new(, 20, 100, z = 30)
  expect_identical(a$dots, list(100))
  expect_identical(a$x, 1)
  expect_identical(a$y, 20)
  expect_identical(a$z, 30)


  A <- R6Class("A", public = list(
    initialize = function(c = 1) {
      self$c = c
    },
    c = NULL
  ))
  # This throws an error because of the name `c`. It also does so if `list` is
  # used.
  A$new()
})
