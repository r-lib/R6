test_that("Subclass can override superclass' cloneable property", {
  # superclass cloneable ---------------------

  Creature <- R6Class("Creature", cloneable = TRUE)

  Sheep <- R6Class("Sheep", inherit = Creature, cloneable = TRUE)
  expect_message(sheep <- Sheep$new(), NA)
  expect_s3_class(sheep$clone(), "Sheep")
  expect_true("clone" %in% names(Creature$public_methods))

  Human <- R6Class("Human", inherit = Creature, cloneable = FALSE)
  expect_message(human <- Human$new(), NA)
  expect_error(human$clone(), "attempt to apply non-function")
  expect_true("clone" %in% names(Creature$public_methods))

  # superclass non-cloneable  ---------------------

  Creature <- R6Class("Creature", cloneable = FALSE)

  Sheep <- R6Class("Sheep", inherit = Creature, cloneable = TRUE)
  expect_message(sheep <- Sheep$new(), "Superclass Creature has cloneable=FALSE, but subclass Sheep has cloneable=TRUE.")
  expect_error(sheep$clone(), "attempt to apply non-function")
  expect_false("clone" %in% names(Creature$public_methods))

  Human <- R6Class("Human", inherit = Creature, cloneable = FALSE)
  expect_message(human <- Human$new(), NA)
  expect_error(human$clone(), "attempt to apply non-function")
  expect_false("clone" %in% names(Creature$public_methods))
})
