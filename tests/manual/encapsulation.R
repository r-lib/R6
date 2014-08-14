library(pryr)
library(testthat)
library(inline)

unlockEnvironment <- cfunction(signature(env = "environment"), body = '
  #define FRAME_LOCK_MASK (1<<14)
  #define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
  #define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

  if (TYPEOF(env) == NILSXP)
    error("use of NULL environment is defunct");
  if (TYPEOF(env) != ENVSXP)
    error("not an environment");

  UNLOCK_FRAME(env);

  // Return TRUE if unlocked; FALSE otherwise
  SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
  LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
  UNPROTECT(1);

  return result;
')

# To make sure these tests actually work:
#   * Un-encapsulate one or more of the encapsulated functions.
#   * load_all(), or install R6, restart R, then library(R6).
#   * Run these tests. With the function(s) commented out, there should be an
#     error. With the code restored to normal, there should be no errors.
test_that("R6 objects can be instantiated even when R6 isn't loaded", {
  library(R6)
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
      incz = function() {
        self$z <- self$z + 1
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
      incz = function() {
        self$z <- self$z + 2
      }
    )
  )

  # Remove everything from the R6 namespace
  r6ns <- .getNamespace('R6')
  unlockEnvironment(r6ns)
  rm(list = ls(r6ns), envir = r6ns)

  # Also try unloading R6 namespace. Even this set of commands may not be enough
  # to fully unload the R6 namespace environment, because AC and BC are children
  # of the R6 namespace.
  detach('package:R6', unload = TRUE)
  expect_null(.getNamespace('R6'))
  expect_error(as.environment('package:R6'))
  expect_error(get('R6Class', inherits = TRUE))

  B <- BC$new()

  # Testing overrides
  expect_identical(B$x, 2)       # Public
  expect_identical(B$gety(), 2)  # Private
  expect_identical(B$z, 2)       # Active
  # Calling superclass methods
  expect_identical(B$inc(0), 21)
  expect_identical(B$pinc(0), 21)


  library(R6)
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
      incz = function() {
        self$z <- self$z + 3
      }
    )
  )

  # Remove everything from the R6 namespace
  r6ns <- .getNamespace('R6')
  unlockEnvironment(r6ns)
  rm(list = ls(r6ns), envir = r6ns)

  # Detach and unload R6, then run the tests as usual
  detach('package:R6', unload = TRUE)
  expect_null(.getNamespace('R6'))
  expect_error(as.environment('package:R6'))
  expect_error(get('R6Class', inherits = TRUE))

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



# Encapsulate R6 in new() =======================
# This set of tests requires restarting R

library(R6)
AC <- R6Class("AC",
  portable = FALSE,
  public = list(
    x = 1,
    getx = function() self$x
  )
)

BC <- R6Class("BC",
  portable = FALSE,
  inherit = AC,
  public = list(
    x = 2,
    getx = function() self$x
  )
)

save(AC, BC, file = 'test.rda')


#### Restart R ####

library(testthat)
load('test.rda')
# R6 will be loaded
expect_true("R6" %in% loadedNamespaces())

A <- AC$new()
B <- BC$new()
expect_identical(A$getx(), 1)
expect_identical(B$getx(), 2)

# Clean up
unlink('test.rda')
