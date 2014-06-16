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
                                      methods = NULL, inherit = NULL,
                                      parent_env = parent.frame()) {

  if (!all(vapply(methods, is.function, logical(1)))) {
    stop("Objects in methods must all be functions.")
  }

  if (!is.null(inherit)) {
    if (!inherits(inherit, "ExternalMethodClassGenerator")) {
      stop("`inherit` must be a ExternalMethodClassGenerator.")
    }

    # Merge the new items over the inherited ones
    members <- merge_vectors(inherit$members, members)
    methods <- merge_vectors(inherit$methods, methods)

    # Do some preparation work on the superclass, so that we don't have to do
    # it each time an object is created.
    super_list <- listify_superclass(inherit)
  } else {
    super_list <- NULL
  }

  # Enclosing env for methods
  methods <- assign_func_envs(methods, parent_env)
  # Binding env for methods
  methods_env <- new.env(parent = emptyenv(), hash = length(methods) > 100)
  # Turn methods into an environment so that it's possible to add methods later
  list2env(methods, envir = methods_env)

  classes <- c(classname, get_superclassnames(inherit), "ExternalMethodClass")

  newfun <- function(...) {
    class(members) <- c(classname, "ExternalMethodClass")
    attr(members, "methods") <- methods_env

    if (is.function(methods$initialize)) {
      members <- methods$initialize(members, ...)
    }
    members
  }

  structure(
    list(new = newfun, classname = classname, members = members,
         methods = methods, inherit = inherit),
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
