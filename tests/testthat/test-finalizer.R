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


test_that("Finalizers are inherited, portable", {

  AC <- R6Class(
    "AC",
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted\n")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted\n")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    )
  )

  CC <- R6Class(
    "CC",
    inherit = BC,
    public = list(
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
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted\n")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted\n")
    )
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
      finalize = function() {
        super$finalize()
        cat("A BC was just deleted\n")
      }
    )
  )

  CC <- R6Class(
    "CC",
    inherit = BC,
    public = list(
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
    public = list(
      finalize = function() cat("An AC was just deleted\n")
    ),
    portable = FALSE
  )

  BC <- R6Class(
    "BC",
    inherit = AC,
    public = list(
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
    public = list(
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
