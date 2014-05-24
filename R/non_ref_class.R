#' Create a class with non-reference semantics
#' @export
createNonRefClass <- function(classname = NULL, members = list(),
                            methods = NULL) {

  methods <- list2env(methods)

  newfun <- function(...) {
    if (is.function(methods$initialize)) {
      members <- methods$initialize(members, ...)
    }

    class(members) <- c(classname, "NonRefClass")
    attr(members, "methods") <- methods
    members
  }

  structure(
    list(new = newfun, classname = classname, members = members,
         methods = methods),
    class = "NonRefClassGenerator"
  )
}

#' @export
`$.NonRefClass` <- function(x, name) {
  if (name %in% names(x)) {
    return(.subset2(x, name))

  } else {
    fun <- attr(x, "methods")[[name]]
    if (is.function(fun)) {
      return(function(...) fun(x, ...))
    }
    NULL
  }
}

#' @export
`[[.NonRefClass` <- `$.NonRefClass`
