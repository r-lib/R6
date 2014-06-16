context("ext-methodclass")

test_that("initialization", {
  AC <- createExternalMethodClass("AC",
    members = list(x = 1),
    methods = list(
      initialize = function(self, x) {
        self <- self$setx2(2)  # Call another class method
        self
      },
      setx2 = function(self, x) {
        self$x <- x * 2
        self
      }
    )
  )
  A <- AC$new(2)
  expect_identical(A$x, 4)
})


test_that("object contains members but not methods", {
  AC <- createExternalMethodClass("AC",
    members = list(x = 1),
    methods = list(
      getx = function(self, x) {
        self$x
      }
    )
  )
  A <- AC$new()
  expect_identical(A$x, 1)
  expect_false("getx" %in% names(A))
})


test_that("object is not a reference object", {
  AC <- createExternalMethodClass("AC",
    members = list(x = 1),
    methods = list(
      setx = function(self, x) {
        self$x <- x
        self
      }
    )
  )
  A <- AC$new()
  expect_identical(A$setx(2)$x, 2)
  expect_identical(A$x, 1)
})


test_that("Validity checks on creation", {
  # All arguments must be named
  expect_error(createExternalMethodClass("AC", members = list(1)))
  expect_error(createExternalMethodClass("AC", methods = list(1)))

  # Names can't be duplicated
  expect_error(createExternalMethodClass("AC", members = list(a=1, a=2)))
  expect_error(createExternalMethodClass("AC", members = list(a=1),
                                         methods = list(a = function() 1)))

  # Reserved names
  expect_error(createRefClass("AC", members = list(self = 1)))
  expect_error(createRefClass("AC", members = list(super = 1)))
})


test_that("methods require 'self' to find each other", {
  # It would be nice within a method to be able to use gety() instead of
  # self$gety(), but it's not possible because we to automatically pass
  # `self` to the call, and this is handled by $.ExternalMethodClass.
  gety <- function() 0
  AC <- createExternalMethodClass("AC",
    members = list(x = 1, y = 10),
    methods = list(
      getx = function(self) self$x,
      gety = function(self) self$y,
      sumxy = function(self) {
        self$getx() + gety()    # Shouldn't find self$gety
      }
    )
  )
  A <- AC$new()
  expect_identical(A$sumxy(), 1)

  # The method runs in this environment
  expect_identical(environment(attr(A, "methods")$sumxy), environment())
})


test_that("inheritance", {
  AC <- createExternalMethodClass("AC",
    members = list(x = 0, x2 = 1, y = 1),
    methods = list(
      initialize = function(self, x = 1) {
        self <- self$setx(x)    # This function is not overridden in Bc
        self$x2 <- self$getx()  # This function is overridden in BC
        self
      },
      setx = function(self, x) { self$x <- x; self },
      getx = function(self) self$x
    )
  )
  BC <- createExternalMethodClass("BC",
    inherit = AC,
    members = list(x = 2, y = 2),
    methods = list(
      getx = function(self) self$x + 20
    )
  )
  B <- BC$new()
  expect_identical(B$x, 1)
  expect_identical(B$x2, 21)     # self$getx() was overridden, called from initialize
  expect_identical(B$y, 2)       # Overrides AC's initial value of y
  expect_identical(B$getx(), 21) # getx() was overridden
})


test_that("inheritance with $super ", {
  AC <- createExternalMethodClass("AC",
    members = list(x = 0, y = 0),
    methods = list(
      initialize = function(self, x = 1) {
        self$x <- x
        self$y <- self$newy()
        self
      },
      getx = function(self) self$x,
      newy = function(self) 1
    )
  )
  BC <- createExternalMethodClass("BC",
    inherit = AC,
    methods = list(
      initialize = function(self, x = 2, super) {
        super$initialize(x + 20)
      },

      getx = function(self, x, super) super$getx() + 20,
      newy = function(self) 20
    )
  )
  B <- BC$new()
  expect_identical(B$x, 22)
  expect_identical(B$y, 20)
  expect_identical(B$getx(), 42)

  CC <- createExternalMethodClass("CC",
    inherit = BC,
    methods = list(
      initialize = function(self, x = 3, super) super$initialize(x + 300),
      getx = function(self, x, super) super$getx() + 300,
      newy = function(self) 300
    )
  )
  C <- CC$new()
  expect_identical(C$x, 323)
  expect_identical(C$y, 300)
  expect_identical(C$getx(), 643)
})
