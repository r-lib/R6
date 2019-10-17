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
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = "", b = )))
})

test_that("new() args with initialize() with dots", {
  Initializer <- R6Class("Initializer", public = list(initialize = function(a = "", ..., b) NULL))
  expect_identical(formals(Initializer$new), as.pairlist(alist(a = "", ... = , b = )))
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
      initialize = function(a, b = 1) {
        self$missing_a <- missing(a)
        self$missing_b <- missing(b)
      },
      missing_a = NULL,
      missing_b = NULL
    )
  )

  x <- Missing$new()
  expect_true(x$missing_a)
  # It would be nice if this were TRUE, but currently I don't see a
  # straightforward way to make that work, and it's extremely rare that a
  # parameter would have a default value AND a check for missing.
  expect_false(x$missing_b)


  MissingSubclass <- R6Class("MissingSubclass", inherit = Missing)
  x <- MissingSubclass$new()
  expect_true(x$missing_a)
  # Missingness is correctly passed along in this case, because
  # MissingSubclass$new() takes `...`.
  expect_true(x$missing_b)
})
