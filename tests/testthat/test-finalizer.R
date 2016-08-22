context("finalizer")


test_that("Finalizers are called, portable", {
  parenv <- new.env()
  parenv$peekaboo <- FALSE
  AC <- R6Class("AC",
    public = list(finalize = function() peekaboo <<- TRUE),
    portable = TRUE,
    parent_env = parenv
  )
  a <- AC$new()
  rm(a)
  gc()
  expect_true(parenv$peekaboo)
})


test_that("Finalizers are called, non-portable", {
  parenv <- new.env()
  parenv$peekaboo <- FALSE
  AC <- R6Class("AC",
    public = list(finalize = function() peekaboo <<- TRUE),
    portable = FALSE,
    parent_env = parenv
  )
  a <- AC$new()
  rm(a)
  gc()
  expect_true(parenv$peekaboo)
})


test_that("Finalizers have the right environment, portable", {
  parenv <- new.env()
  parenv$pub <- parenv$priv <- FALSE
  AC <- R6Class(
    "AC",
    public = list(
      finalize = function() { pub <<- self$mypub; priv <<- private$mypriv },
      mypub = TRUE
    ),
    private = list(
      mypriv = TRUE
    ),
    portable = TRUE,
    parent_env = parenv
  )
  a <- AC$new()
  rm(a)
  gc()
  expect_true(parenv$pub)
  expect_true(parenv$priv)
})


test_that("Finalizers have the right environment, non-portable #1", {
  parenv <- new.env()
  parenv$pub <- parenv$priv <- FALSE
  AC <- R6Class(
    "AC",
    public = list(
      finalize = function() { pub <<- self$mypub; priv <<- private$mypriv },
      mypub = TRUE
    ),
    private = list(
      mypriv = TRUE
    ),
    portable = FALSE,
    parent_env = parenv
  )
  a <- AC$new()
  rm(a)
  gc()
  expect_true(parenv$pub)
  expect_true(parenv$priv)
})


test_that("Finalizers have the right environment, non-portable #2", {
  parenv <- new.env()
  parenv$pub <- parenv$priv <- FALSE
  AC <- R6Class(
    "AC",
    public = list(
      finalize = function() { pub <<- mypub; priv <<- mypriv },
      mypub = TRUE
    ),
    private = list(
      mypriv = TRUE
    ),
    portable = FALSE,
    parent_env = parenv
  )
  a <- AC$new()
  rm(a)
  gc()
  expect_true(parenv$pub)
  expect_true(parenv$priv)
})
