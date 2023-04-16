test_that("list of public members is generated as expected by as.list.R6 method", {
  Person <- R6Class("Person",
    public = list(
      name = NULL,
      hair = NULL,
      initialize = function(name = NA, hair = NA) {
        self$name <- name
        self$hair <- hair
      },
      set_hair = function(val) {
        self$hair <- val
      }
    )
  )

  ann <- Person$new("Ann", "black")

  annList <- as.list(ann)

  expect_type(annList, "list")
  expect_equal(
    names(annList),
    c(".__enclos_env__", "hair", "name", "clone", ".clone", "set_hair", "initialize")
  )

  expect_equal(annList$hair, ann$hair)
  expect_equal(annList$name, ann$name)
})
