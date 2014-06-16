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
