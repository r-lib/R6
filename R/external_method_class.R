#' Create a class with non-reference semantics and externally-stored methods
#'
#' @param classname Name of the class.
#' @param members A list of members, which can be functions and non-functions.
#' @param methods A list of methods for the class.
#' @param inherit A ExternalMethodClass object to inherit from (a superclass).
#' @param lock Should the methods for this class be locked? If locked, it won't
#'   be possible to add more methods later.
#' @param parent_env The enclosing environment to use for the methods. If
#'   \code{NULL}, keep the methods' existing enclosing environment.
#' @export
#' @examples
#' AnimalHerd <- createExternalMethodClass("AnimalHerd",
#'   members = list(
#'     animal = "buffalo",
#'     count = 0
#'   ),
#'   methods = list(
#'     initialize = function(self, count = 0) {
#'       self$count <- count
#'       self
#'     },
#'     view = function(self) {
#'       paste(rep(self$animal, self$count), collapse = " ")
#'     },
#'     reproduce = function(self, mult = 2) {
#'       self$count <- self$count * mult
#'       invisible(self)
#'     }
#'   ),
#'   lock = FALSE
#' )
#'
#' herd <- AnimalHerd$new(2)
#' herd$view()
#' # "buffalo buffalo"
#'
#' herd$reproduce()
#' # No change to herd because it doesn't have reference semantics
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
#' # "buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo"
#'
#' # Can add methods after the class has already been created, because we
#' # used lock = FALSE
#' AnimalHerd$methods$grow <- function(self) {
#'   self$animal <- toupper(self$animal)
#'   self
#' }
#'
#' herd$grow()$view()
#' # "BUFFALO BUFFALO BUFFALO BUFFALO"
#'
#'
#' # Inheritance
#' Person <- createExternalMethodClass("Person",
#'   members = list(
#'     name = NA,
#'     hair = NA
#'   ),
#'   methods = list(
#'     initialize = function(self, name, hair = NA) {
#'       self$name <- name
#'       self$hair <- hair
#'       self$greet()
#'       self
#'     },
#'     greet = function(self) {
#'       cat(paste0("Hello, my name is ", self$name, ".\n"))
#'     }
#'   )
#' )
#' ann <- Person$new("Ann", "black")
#' # Hello, my name is Ann.
#' ann$hair
#' # "black"
#'
#'
#' Lumberjack <- createExternalMethodClass("Lumberjack",
#'   inherit = Person,
#'   members = list(
#'     beard = NA
#'   ),
#'   methods = list(
#'     initialize = function(self, name, hair = NA, beard = NA, super) {
#'       self <- super$initialize(name, hair)
#'       self$beard <- beard
#'       self
#'     },
#'     greet = function(self) {
#'       cat(paste("I'm a lumberjack and I'm OK.\n"))
#'     }
#'   )
#' )
#' jim <- Lumberjack$new("Jim", "red", "bushy")
#' # I'm a lumberjack and I'm OK.
#' jim$hair
#' # "red"
#' jim$beard
#' # "bushy"
createExternalMethodClass <- function(classname = NULL, members = list(),
                                      methods = NULL, inherit = NULL,
                                      lock = TRUE, parent_env = NULL) {

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
  list2env2(methods, envir = methods_env)
  if (lock) {
    lockEnvironment(methods_env)
  }

  classes <- c(classname, get_superclassnames(inherit), "ExternalMethodClass")

  class(members) <- c(classname, "ExternalMethodClass")
  attr(members, "methods") <- methods_env

  newfun <- externalMethodsClass_newfun(members)

  structure(
    list(new = newfun, classname = classname, members = members,
         methods = methods_env, inherit = inherit),
    class = "ExternalMethodClassGenerator"
  )
}

# Return a $new function for the ExternalMethodClassGenerator
externalMethodsClass_newfun <- function(self) {
  function(...) {
    if (is.function(self$initialize)) {
      self <- self$initialize(...)
    } else if (length(list(...)) != 0 ) {
      stop("Called new() with arguments, but there is no initialize method.")
    }
    self
  }
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
        return(function(...) fun(self = x, ..., super = super))
      } else {
        return(function(...) fun(self = x, ...))
      }
    }
    NULL
  }
}

#' @export
`[[.ExternalMethodClass` <- `$.ExternalMethodClass`

# A special object that's created when calling superclass methods
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
      return(function(...) fun(self = self, ..., super = super))
    } else {
      return(function(...) fun(self = self, ...))
    }
  }
  NULL
}

#' @export
`[[.ExternalSuperMethods` <- `$.ExternalSuperMethods`
