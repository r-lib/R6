context("aslist")

test_that("as.list() works as expected", {
  AC <- R6Class("AC", public = list(x = 1))
  ac <- AC$new()
  lst <- as.list(ac)
  expect_true(methods::is(lst, "list"))
  expect_named(lst, c(".__enclos_env__", "x", "clone"))
  expect_true(lst[["x"]] == 1)
})
