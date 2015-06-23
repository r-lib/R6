context("set")

test_that("Setting values set values on generator", {
  AC <- R6Class("AC",
    public = list(
      x = 1,
      getxyz = function() self$x + private$y + private$z()
    ),
    private = list(
      y = 2,
      z = function() 3
    ),
    active = list(
      x2 = function(value) {
        if (missing(value)) return(self$x * 2)
        else self$x <<- value/2
      }
    )
  )

  # Can set new names
  AC$set("public", "nx", 10)
  AC$set("public", "ngetxyz", function() self$nx + private$ny + private$nz())
  AC$set("private", "ny", 20)
  AC$set("private", "nz", function() 30)
  AC$set("active", "nx2", function(value) {
    if (missing(value)) return(self$nx * 2)
    else self$nx <<- value/2
  })

  A <- AC$new()
  expect_identical(A$nx, 10)
  expect_identical(A$ngetxyz(), 60)
  expect_identical(A$nx2, 20)


  # Can't set existing names
  expect_error(AC$set("public", "x", 99))
  expect_error(AC$set("public", "getxyz", function() 99))
  expect_error(AC$set("private", "y", 99))
  expect_error(AC$set("private", "z", function() 99))
  expect_error(AC$set("active", "x2", function(value) 99))

  # Can't set existing names in different group
  expect_error(AC$set("private", "x", 99))
  expect_error(AC$set("private", "getxyz", function() 99))
  expect_error(AC$set("active", "y", 99))
  expect_error(AC$set("public", "z", function() 99))
  expect_error(AC$set("private", "x2", function(value) 99))

  # Can set existing names if overwrite = TRUE
  AC$set("public", "x", 99, overwrite = TRUE)
  AC$set("public", "getxyz", function() 99, overwrite = TRUE)
  AC$set("private", "y", 99, overwrite = TRUE)
  AC$set("private", "z", function() 99, overwrite = TRUE)
  AC$set("active", "x2", function(value) 99, overwrite = TRUE)

  # Can't set existing names in different group, even if overwrite = TRUE
  expect_error(AC$set("private", "x", 99, overwrite = TRUE))
  expect_error(AC$set("private", "getxyz", function() 99, overwrite = TRUE))
  expect_error(AC$set("active", "y", 99, overwrite = TRUE))
  expect_error(AC$set("public", "z", function() 99, overwrite = TRUE))
  expect_error(AC$set("private", "x2", function(value) 99, overwrite = TRUE))
})


test_that("Setting values with empty public or private", {
  AC <- R6Class("AC",
    public = list(),
    private = list()
  )
  AC$set("public", "x", 1)
  AC$set("private", "y", 1)
  AC$set("public", "gety", function() private$y)

  a <- AC$new()
  expect_identical(a$x, 1)
  expect_identical(a$gety(), 1)
})

test_that("Locked class", {
  AC <- R6Class("AC", lock_class = TRUE)
  expect_error(AC$set("public", "x", 1))
  expect_error(AC$set("private", "x", 1))

  expect_true(AC$is_locked())
  AC$unlock()
  expect_false(AC$is_locked())
  AC$set("public", "x", 1)
  AC$lock()
  expect_error(AC$set("public", "x", 2))
})
