getImplementClassname <- function(x) {
  # gen <- try(getAnywhere(class(x)[1]), silent = TRUE)
  gen <- try(get(class(x)[1]), silent = TRUE)
  if (!inherits(gen, "try-error") &&
      !is.null(implement <- gen$get_implement())
  ) {
    implement$classname
  }
}