test_that("Subclass can override superclass' cloneable property", {
  msg <- "Sub and superclass have different cloneable properties."

  # superclass cloneable ---------------------

  Creature <- R6Class("Creature", cloneable = TRUE)

  Sheep <- R6Class("Sheep", inherit = Creature, cloneable = TRUE)
  expect_message(sheep <- Sheep$new(), NA)
  expect_s3_class(sheep$clone(), "Sheep")

  Human <- R6Class("Human", inherit = Creature, cloneable = FALSE)
  expect_message(human <- Human$new(), msg)
  expect_error(human$clone(), "attempt to apply non-function")

  # superclass non-cloneable  ---------------------

  Creature <- R6Class("Creature", cloneable = FALSE)

  Sheep <- R6Class("Sheep", inherit = Creature, cloneable = TRUE)
  expect_message(sheep <- Sheep$new(), msg)
  expect_s3_class(sheep$clone(), "Sheep")

  Human <- R6Class("Human", inherit = Creature, cloneable = FALSE)
  expect_message(human <- Human$new(), NA)
  expect_error(human$clone(), "attempt to apply non-function")
})
