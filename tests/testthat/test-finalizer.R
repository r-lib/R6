# context("finalizer")


test_that("Finalizers are called, portable", {
  parenv <- new.env()
  parenv$peekaboo <- FALSE
  AC <- R6Class("AC",
    private = list(finalize = function() peekaboo <<- TRUE),
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
    private = list(finalize = function() peekaboo <<- TRUE),
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
      mypub = TRUE
    ),
    private = list(
      finalize = function() { pub <<- self$mypub; priv <<- private$mypriv },
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
      mypub = TRUE
    ),
    private = list(
      finalize = function() { pub <<- self$mypub; priv <<- private$mypriv },
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
      mypub = TRUE
    ),
    private = list(
      finalize = function() { pub <<- mypub; priv <<- mypriv },
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


test_that("Finalizers are inherited, portable", {

  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() print("An AC was just deleted")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC
  )

  B <- BC$new()
  expect_output({ rm(B); gc() }, "An AC was just deleted")
})


test_that("Children can override finalizers, portable", {

  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() cat("A BC was just deleted")
    )
  )

  B <- BC$new()
  ## The anchors make sure that there is no extra output here
  expect_output({ rm(B); gc() }, "^A BC was just deleted$")
})


test_that("Children can call finalizers in the parent, portable", {

  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted\n")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    )
  )

  B <- BC$new()
  expect_output(
    { rm(B); gc() },
    "An AC was just deleted.*A BC was just deleted"
  )
})


test_that("Finalizers and two levels of inheritance, portable", {
  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted\n")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    )
  )

  CC <- R6Class(
    "CC",
    inherit = BC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A CC was just deleted\n")
      }
    )
  )

  C <- CC$new()
  expect_output(
    { rm(C); gc() },
    "An AC was just deleted.*A BC was just deleted.*A CC was just deleted"
  )
})


test_that("Finalizers are inherited, non-portable", {

  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() print("An AC was just deleted")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    portable = FALSE
  )

  B <- BC$new()
  expect_output({ rm(B); gc() }, "An AC was just deleted")
})


test_that("Children can override finalizers, non-portable", {

  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() cat("A BC was just deleted")
    ),
    portable = FALSE
  )

  B <- BC$new()
  ## The anchors make sure that there is no extra output here
  expect_output({ rm(B); gc() }, "^A BC was just deleted$")
})


test_that("Children can call finalizers in the parent, non-portable", {

  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted\n")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    ),
    portable = FALSE
  )

  B <- BC$new()
  expect_output(
    { rm(B); gc() },
    "An AC was just deleted.*A BC was just deleted"
  )
})


test_that("Finalizers and two levels of inheritance, portable", {
  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted\n")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    )
  )

  CC <- R6Class(
    "CC",
    inherit = BC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A CC was just deleted\n")
      }
    )
  )

  C <- CC$new()
  expect_output(
    { rm(C); gc() },
    "An AC was just deleted.*A BC was just deleted.*A CC was just deleted"
  )
})

test_that("Finalizers and two levels of inheritance, non-portable", {
  AC <- R6Class(
    "AC",
    private = list(
      finalize = function() cat("An AC was just deleted\n")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    ),
    portable = FALSE
  )

  CC <- R6Class(
    "CC",
    inherit = BC,
    private = list(
      finalize = function() {
        super$finalize()
        cat("A CC was just deleted\n")
      }
    ),
    portable = FALSE
  )

  C <- CC$new()
  expect_output(
    { rm(C); gc() },
    "An AC was just deleted.*A BC was just deleted.*A CC was just deleted"
  )
})


# Issue #121
test_that("Finalizer method does not prevent GC of objects passed to initialize", {
  a_gc <- 0
  A <- R6Class(
    "A",
    public = list(
      initialize = function(x) {
        force(x) # Need to eval x
      }
    ),
    private = list(
      finalize = function(e) {
        a_gc <<- a_gc + 1
      }
    )
  )

  x_gc <- 0
  x <- new.env(parent = emptyenv())
  reg.finalizer(x, function(e) { x_gc <<- x_gc + 1 })

  # Pass x to A's initialize method
  a <- A$new(x)

  rm(x)
  gc()
  expect_identical(x_gc, 1)  # This is the key test: x should be GC'd

  rm(a)
  gc()
  expect_identical(a_gc, 1)


  # Same test, but with clone
  a_gc <- 0
  x_gc <- 0
  x <- new.env(parent = emptyenv())
  reg.finalizer(x, function(e) { x_gc <<- x_gc + 1 })

  # Pass x to A's initialize method
  a <- A$new(x)
  b <- a$clone()

  rm(x)
  gc()
  expect_identical(x_gc, 1)  # This is the key test: x should be GC'd

  rm(a)
  gc()
  expect_identical(a_gc, 1)
  rm(b)
  gc()
  expect_identical(a_gc, 2)

  expect_identical(x_gc, 1)  # Make sure x's finalizer hasn't somehow run again
})


test_that("Public finalizers emit message", {
  expect_message(
    R6Class("C1",
      public = list(
        x = 1,
        finalize = function() NULL
      )
    )
  )
})
