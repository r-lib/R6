# R's JIT shares one compiled copy of a method body between closures, and
# never compiles a closure whose environment binds a name the compiled one
# lacked (https://bugs.r-project.org/show_bug.cgi?id=19308). So each copy of a
# method should run in an enclosing environment that binds the same names,
# whichever object it belongs to.

enclos_names <- function(f) {
  sort(ls(environment(f), all.names = TRUE))
}

test_that("Inherited methods have the same enclosing names as in the superclass", {
  Base <- R6Class("Base", public = list(f = function() 1))
  Sub <- R6Class("Sub", inherit = Base)
  SubPrivate <- R6Class("SubPrivate", inherit = Base, private = list(x = 1))
  SubActive <- R6Class("SubActive", inherit = Base, active = list(a = function() 1))
  GrandChild <- R6Class("GrandChild", inherit = SubPrivate)

  base <- Base$new()
  objs <- list(Sub$new(), SubPrivate$new(), SubActive$new(), GrandChild$new())
  objs <- c(objs, base$clone(), lapply(objs, function(x) x$clone()))

  expect_identical(enclos_names(base$f), c("private", "self"))
  for (obj in objs) {
    expect_identical(enclos_names(obj$f), enclos_names(base$f))
  }
})

test_that("Inherited methods match when the superclass has private members", {
  Base <- R6Class("Base", public = list(f = function() private$x), private = list(x = 1))
  Sub <- R6Class("Sub", inherit = Base)

  base <- Base$new()
  sub <- Sub$new()
  expect_identical(enclos_names(sub$f), enclos_names(base$f))
  expect_identical(enclos_names(sub$clone()$f), enclos_names(base$f))
  expect_identical(sub$f(), 1)
})

test_that("Methods of a subclass match in objects of its own subclasses", {
  Base <- R6Class("Base", public = list(f = function() 1))
  Sub <- R6Class("Sub", inherit = Base, public = list(g = function() super$f()))
  SubSub <- R6Class("SubSub", inherit = Sub, private = list(x = 1))

  sub <- Sub$new()
  subsub <- SubSub$new()
  expect_identical(enclos_names(sub$g), c("private", "self", "super"))
  expect_identical(enclos_names(subsub$g), enclos_names(sub$g))
  expect_identical(subsub$g(), 1)
})

test_that("Classes without private members bind private to NULL", {
  AC <- R6Class("AC", public = list(getprivate = function() private))
  BC <- R6Class("BC", inherit = AC)

  expect_null(AC$new()$getprivate())
  expect_null(BC$new()$getprivate())
  expect_null(AC$new()$clone()$getprivate())
})

test_that("Non-portable classes do not gain a private binding", {
  AC <- R6Class("AC", portable = FALSE, public = list(f = function() 1))
  BC <- R6Class("BC", portable = FALSE, inherit = AC)

  expect_false("private" %in% ls(AC$new(), all.names = TRUE))
  expect_false("private" %in% ls(BC$new(), all.names = TRUE))
  expect_false("private" %in% ls(BC$new()$clone(), all.names = TRUE))
})
