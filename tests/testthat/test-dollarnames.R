test_that(".DollarNames works as expected", {
  AC <- R6Class("AC",
    public = list(
      x = 1,
      .y = 1
    ),
    private = list(
      px = 1,
      .py = 1
    ),
    active = list(
      ax = function(value) 1,
      .ay = function(value) 1
    )
  )

  a <- AC$new()
  expected_names <- c("x", ".y", "ax", ".ay", "clone")
  expect_setequal(.DollarNames(a, ""), expected_names)
  expect_setequal(utils:::.DollarNames(a, ""), expected_names)

  # Tests for direct calling of .DollarNames.R6 without S3 dispatch
  # https://github.com/rstudio/rstudio/issue/15706
  # https://github.com/rstudio/rstudio/pull/15707
  expect_setequal(R6:::.DollarNames.R6(a, ""), expected_names)
  DollarNamesR6 <- R6:::.DollarNames.R6
  expect_setequal(DollarNamesR6(a, ""), expected_names)
})
