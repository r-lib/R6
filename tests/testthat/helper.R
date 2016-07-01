expect_no_error <- function(expr) {
  err <- FALSE
  tryCatch(force(expr),
    error = function(e) {
      err <<- TRUE
    }
  )
  expect(!err, "Expected no error, but had error.")
  invisible(NULL)
}