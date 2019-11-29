# This is the $new function for a R6ClassGenerator. This copy of it won't run
# properly; it needs to be copied, and its parent environment set to the
# generator object environment.
generator_funs$new <- function(...) {

  # Instantiate a new object from the generator attached method, which can then
  # be initialized
  public_bind_env <-
    get(".__instantiate", envir = parent.env(environment()))()

  # Initialize ------------------------------------------------------
  initialize <- .subset2(public_bind_env, "initialize")
  if (is.function(initialize)) {
    initialize(...)
  } else if (length(list(...)) != 0 ) {
    stop("Called new() with arguments, but there is no initialize method.")
  }

  public_bind_env
}
