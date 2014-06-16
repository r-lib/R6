#' Create a class with non-reference semantics and externally-stored methods
#' @export
#' @examples
#' AnimalHerd <- createExternalMethodClass("AnimalHerd",
#'   members = list(
#'     animal = "buffalo",
#'     count = 2
#'   ),
#'   methods = list(
#'     view = function(self) {
#'       paste(rep(self$animal, self$count), collapse = " ")
#'     },
#'     reproduce = function(self, mult = 2) {
#'       self$count <- self$count * mult
#'       invisible(self)
#'     }
#'   )
#' )
#'
#' herd <- AnimalHerd$new()
#' herd$view()
#' # "buffalo buffalo"
#'
#' herd$reproduce()
#' # No change because it doesn't have reference semantics
#' herd$view()
#' # "buffalo buffalo"
#'
#' # Need to save it back into the variable if we want to store the result
#' herd <- herd$reproduce()
#' herd$view()
#' # "buffalo buffalo buffalo buffalo"
#'
#' # Methods that return self are chainable
#' herd$reproduce()$view()
#' "buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo"
#'
#'
#' # Can add methods after the class has already been created
#' AnimalHerd$methods$grow <- function(self) {
#'   self$animal <- toupper(self$animal)
#'   self
#' }
#'
#' herd$grow()$view()
#' "BUFFALO BUFFALO BUFFALO BUFFALO"
createExternalMethodClass <- function(classname = NULL, members = list(),
                                      methods = NULL) {

  if (!all(vapply(methods, is.function, logical(1)))) {
    stop("Objects in methods must all be functions.")
  }

  # Turn methods into an environment so that it's possible to add methods later
  methods <- list2env(methods)

  newfun <- function(...) {
    class(members) <- c(classname, "ExternalMethodClass")
    attr(members, "methods") <- methods

    if (is.function(methods$initialize)) {
      members <- methods$initialize(members, ...)
    }
    members
  }

  structure(
    list(new = newfun, classname = classname, members = members,
         methods = methods),
    class = "ExternalMethodClassGenerator"
  )
}

#' @export
`$.ExternalMethodClass` <- function(x, name) {
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
`[[.ExternalMethodClass` <- `$.ExternalMethodClass`
