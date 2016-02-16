context("interfaces")

test_that("Test generator object structure", {
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  Foo <- R6Class("Foo", implement = IFoo,
    public = list(foo = function(n = 1) private$x[1:n]),
    private = list(x = letters)
  )
  expect_true(exists("implement", Foo))
  expect_true(identical(Foo$implement, as.name("IFoo")))
  expect_true(exists("get_implement", Foo))
  expect_true(identical(Foo$get_implement(), IFoo))
})

test_that("Interface implemented correctly", {
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  Foo <- R6Class("Foo", implement = IFoo,
    public = list(foo = function(n = 1) private$x[1:n]),
    private = list(x = letters)
  )
  expect_is(inst <- Foo$new(), "Foo")
  expect_true(inherits(inst, "IFoo"))
})

test_that("Interface implemented incorrectly", {
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  Foo <- R6Class("Foo", implement = IFoo,
    private = list(x = letters)
  )
  expect_error(Foo$new(), "Non-implemented interface method: foo")
})

test_that("Interface and standard inheritance", {
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  BaseClass <- R6Class("BaseClass",
    public = list(foo = function(n = 1) private$x[1:n])
  )
  Foo <- R6Class("Foo", implement = IFoo, inherit = BaseClass,
    private = list(x = letters)
  )
  expect_is(inst <- Foo$new(), "Foo")
  expect_true(inherits(inst, "BaseClass"))
  expect_true(inherits(inst, "IFoo"))

  expect_identical(inst$foo(3), letters[1:3])
})

context("interfaces: print method")

test_that("Test print method: standard inheritance", {
  skip("Manual only. Print helper `getImplementClassname` still off with
    respect to enclosing frames")
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  Foo <- R6Class("Foo", inherit = IFoo,
    public = list(foo = function(n = 1) private$x[1:n]),
    private = list(x = letters)
  )
  expect_true(any(grepl("Inherits from: <IFoo>", capture.output(Foo))))

  inst <- Foo$new()
  print(capture.output(inst))
  expect_true(any(grepl("Inherits from: <IFoo>", capture.output(inst))))
})

test_that("Test print method: interface implementation", {
  # skip("Manual only. Print helper `getImplementClassname` still off with
  #   respect to enclosing frames")
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  Foo <- R6Class("Foo", implement = IFoo,
    public = list(foo = function(n = 1) private$x[1:n]),
    private = list(x = letters)
  )
  expect_true(any(grepl("Implements interface: <IFoo>", capture.output(Foo))))

  inst <- Foo$new()
  expect_true(any(grepl("Implements interface: <IFoo>", capture.output(inst))))
})

test_that("Test print method: interface and standard inheritance", {
  IFoo <- R6Class("IFoo",
    public = list(foo = function() stop("I'm the inferace method"))
  )
  BaseClass <- R6Class("BaseClass",
    public = list(foo = function(n = 1) private$x[1:n])
  )
  Foo <- R6Class("Foo", implement = IFoo, inherit = BaseClass,
    private = list(x = letters)
  )
  expect_true(any(grepl("Inherits from: <BaseClass>", capture.output(Foo))))
  expect_true(any(grepl("Implements interface: <IFoo>", capture.output(Foo))))

  inst <- Foo$new()
  expect_true(any(grepl("Inherits from: <BaseClass>", capture.output(inst))))
  expect_true(any(grepl("Implements interface: <IFoo>", capture.output(inst))))
})