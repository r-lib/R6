# This is the enclosing environment for all of the functions involved in
# instantiating objects. It is also the binding environment for all these
# functions, except for R6Class(). This is because a generator object can be
# saved (in a built package, for example) and then restored in a different R
# session which has a different version of the R6 package. With the capsule
# environment, the generator object doesn't need to use any functions or objects
# from the potentially different R6 namespace, and because the saved/restored
# object also saves and restores the capsule environment (but not the R6
# namespace).
capsule <- new.env(hash = FALSE)

# This function takes an expression and evaluates it in the capsule environment.
encapsulate <- function(expr) {
  expr <- substitute(expr)
  eval(expr, capsule)
}

encapsulate({
  # Given two named vectors, join them together, and keep only the last element
  # with a given name in the resulting vector. If b has any elements with the
  # same name as elements in a, the element in a is dropped. Also, if there are
  # any duplicated names in a or b, only the last one with that name is kept.
  merge_vectors <- function(a, b) {
    if ((!is.null(a) && length(a) > 1 && is.null(names(a))) ||
        (!is.null(b) && length(b) > 1 && is.null(names(b)))) {
      stop("merge_vectors: vectors must be either NULL or named vectors")
    }

    x <- c(a, b)
    drop_idx <- duplicated(names(x), fromLast = TRUE)
    x[!drop_idx]
  }

  # Check that all elements of a list are named.
  # NULL and empty lists return TRUE.
  all_named <- function(x) {
    if (length(names(x)) != length(x) || any(names(x) == "")) {
      return(FALSE)
    }
    TRUE
  }

  # Return all the functions in a list.
  get_functions <- function(x) {
    funcs <- vapply(x, is.function, logical(1))
    if (all(!funcs)) return(NULL)
    x[funcs]
  }

  # Return all the non-functions in a list.
  get_nonfunctions <- function(x) {
    funcs <- vapply(x, is.function, logical(1))
    if (all(funcs)) return(NULL)
    x[!funcs]
  }
})
