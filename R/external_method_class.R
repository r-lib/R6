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

  if (!all_named(members) || !all_named(methods)) {
    stop("All elements of members and methods must be named.")
  }
  if (length(get_nonfunctions(methods)) != 0) {
    stop("Objects in methods must all be functions.")
  }
  if (any(duplicated(c(names(members), names(methods))))) {
    stop("All items in members and methods must have unique names.")
  }
  if (any(c(names(members), names(methods)) %in% c("self", "super"))) {
    stop("Items cannot use reserved names 'self' and 'super'.")
  }

  if (!is.null(inherit)) {
    if (!inherits(inherit, "ExternalMethodClassGenerator")) {
      stop("`inherit` must be a ExternalMethodClassGenerator.")
    }

    # Merge the new items over the inherited ones
    members <- merge_vectors(inherit$members, members)
    methods <- merge_vectors(as.list(inherit$methods), methods)

    # Point to the superclass's methods
    methods$super <- inherit$methods
  }

  # Enclosing env for methods
  methods <- assign_func_envs(methods, parent_env)
  # Binding env for methods
  methods_env <- new.env(parent = emptyenv(), hash = length(methods) > 100)
  # Turn methods into an environment so that it's possible to add methods later
  list2env(methods, envir = methods_env)

  classes <- c(classname, get_superclassnames(inherit), "ExternalMethodClass")

  class(members) <- c(classname, "ExternalMethodClass")
  attr(members, "methods") <- methods_env

  newfun <- function(...) {
    if (is.function(members$initialize)) {
      members <- members$initialize(...)
    }
    members
  }

  structure(
    list(new = newfun, classname = classname, members = members,
         methods = methods_env, inherit = inherit),
    class = "ExternalMethodClassGenerator"
  )
}

#' @export
`$.ExternalMethodClass` <- function(x, name) {
  if (name %in% names(x)) {
    return(.subset2(x, name))

  } else {
    methods <- attr(x, "methods")
    fun <- methods[[name]]

    if (is.function(fun)) {
      if ("super" %in% names(formals(fun))) {
        super <- createExternalSuperMethods(x, methods$super)
        return(function(...) fun(x, ..., super = super))
      } else {
        return(function(...) fun(x, ...))
      }
    }
    NULL
  }
}

#' @export
`[[.ExternalMethodClass` <- `$.ExternalMethodClass`


createExternalSuperMethods <- function(obj, methods) {
  super <- list(self = obj, methods = methods)
  class(super) <- "ExternalSuperMethods"
  super
}

#' @export
`$.ExternalSuperMethods` <- function(x, name) {
  self <- .subset2(x, "self")
  methods <- .subset2(x, "methods")
  fun <- .subset2(methods, name)

  if (is.function(fun)) {
    if ("super" %in% names(formals(fun))) {
      super <- createExternalSuperMethods(self, methods$super)
      return(function(...) fun(self, ..., super = super))

    } else {
      return(function(...) fun(self, ...))
    }
  }
  NULL
}

#' @export
`[[.ExternalSuperMethods` <- `$.ExternalSuperMethods`
