#' Create an R6 reference object generator
#'
#' Create a lightweight reference class: an environment containing functions.
#'
#' Classes created by this generator have the following properties:
#' \itemize{
#'   \item They have public members, and optionally have private members and
#'     active bindings.
#'   \item Public members reside in an environment. This environment is what
#'     is returned by the generator, and is sometimes referred to as an R6
#'     object.
#'   \item If there are any private members, they are put in a private
#'     environment, which is the parent of the public environment. The parent
#'     of the private environment is set with the \code{parent_env} argument.
#'   \item If there are no private members, then no private environment is
#'     created, and the parent of the public environment is set with
#'     \code{parent_env}.
#'   \item If present, active bindings are put in the public environment.
#'   \item The generator's \code{$new} method creates a new object and returns
#'     its public environment.
#'   \item Methods can directly access the public and private environments, by
#'     using \code{private$x} or \code{self$x} (for public). Assignment to
#'     either environment can be done with \code{<<-}, but it's more precise to
#'     explicitly specify \code{private} or \code{self}.
#'   \item The enclosing environment of all methods is set to the public
#'     environment, even for private methods. In other words, private methods
#'     are found in the private environment, but when they are called, the
#'     public environment is the parent environment.
#'   \item Each instance of the class has its own copy of each method. The
#'     memory cost of this is small; it should be 56 bytes per method.
#'   \item R6 objects have a class attribute so that thet may be used with S3
#'     methods.
#' }
#'
#' The \code{active} argument is a list of active binding functions. These
#' functions take one argument. They look like regular variables, but when
#' accessed, a function is called with an optional argument. For example,
#' if \code{obj$x2} is an active binding, then when accessed as \code{obj$x2},
#' it calls the \code{x2()} function that was in the \code{active} list, with
#' no arguments. However, if a value is assigned to it, as in
#' \code{obj$x2 <- 50}, then the function is called with the right-side value
#' as its argument, as in \code{x2(50)}.
#'
#' If the public or private lists contain any items that have reference
#' semantics (for example, an environment), those items will be shared across
#' all instances of the class. To avoid this, add an entry for that item with
#' a \code{NULL} initial value, and then in the \code{intialize} method,
#' instantiate the object and assign it.
#'
#' @section S3 details:
#'
#' Normally the public environment will have two classes: the one supplied in
#' the \code{classname} argument, and \code{"R6Class"}. It is possible to
#' get the public environment with no classes, by using \code{class = FALSE}.
#' This will result in faster access speeds by avoiding class-based dispatch
#' of \code{$}. The benefit is is negligible in most cases. With classes,
#' accessing a member with \code{$} takes around 2 microseconds on a modern
#' machine; without classes, it takes around 0.3 microseconds. This will make
#' a noticeable difference in performance only when there are hundreds of
#' thousands or more iterations.
#'
#' The primary difference in behavior when \code{class=FALSE} is that, without
#' a class attribute, it won't be possible to use S3 methods with the objects,
#' and so pretty printing (with \code{print.R6Class}) won't be used.
#'
#' @seealso \code{\link{makeActiveBinding}}
#' @aliases R6
#' @export
#' @param classname Name of the class.
#' @param public A list of public members, which can be functions (methods) and
#'   non-functions (fields).
#' @param private An optional list of private members, which can be functions
#'   and non-functions.
#' @param active An optional list of active binding functions.
#' @param inherit A R6ClassGenerator object to inherit from; in other words,
#'   a superclass.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param class Should a class attribute be added to the public environment?
#'   Default is \code{TRUE}.
#' @param lock Should the environments of the generated objects be locked?
#'   If lcoked, new members can't be added to the objects.
#' @examples
#' # A simple class
#' AnimalHerd <- R6Class("AnimalHerd",
#'   public = list(
#'     animal = "buffalo",
#'     count = 2,
#'     view = function() {
#'       paste(rep(animal, count), collapse = " ")
#'     },
#'     reproduce = function(mult = 2) {
#'       count <<- count * mult
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
#' herd$view()
#' # "buffalo buffalo buffalo buffalo"
#'
#' # Methods that return self are chainable
#' herd$reproduce()$view()
#' "buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo"
#'
#'
#' # An example that demonstrates private members and active bindings
#' MyClass <- R6Class("MyClass",
#'   private = list(
#'     x = 2,
#'     # Private methods can access public members
#'     prod_xy = function() x * y
#'   ),
#'   public = list(
#'     y = 3,
#'     initialize = function(x, y) {
#'       if (!missing(x)) private$x <- x
#'       if (!missing(y)) self$y <- y
#'     },
#'     # Set a private variable
#'     set_x = function(value) private$x <- value,
#'     # Increment y, and return self
#'     inc_y = function(n = 1) {
#'       y <<- y + n
#'       invisible(self)
#'     },
#'     # Access private and public members
#'     sum_xy = function() x + y,
#'     # Access a private variable and private method
#'     sumprod = function() x + prod_xy()
#'   ),
#'   active = list(
#'     y2 = function(value) {
#'       if (missing(value)) return(y * 2)
#'       else self$y <- value/2
#'     }
#'   )
#' )
#'
#' z <- MyClass$new(5)
#'
#' z$sum_xy()   # 8
#' z$sumprod()  # 20
#' # z$x <- 20  # Error - can't access private member directly
#' z$set_x(20)
#' z$sum_xy()   # 23
#' z$y <- 100   # Can set public members directly
#' z$sum_xy()   # 120
#'
#' z$y2         # An active binding that returns y*2
#' z$y2 <- 1000 # Setting an active binding
#' z$y          # 500
#'
#' # Methods that return self allow chaining
#' z$inc_y()$inc_y()
#' z$y          # 502
#'
#' # Print, using the print.R6Class method:
#' print(z)
R6Class <- function(classname = NULL, public = list(),
                    private = NULL, active = NULL,
                    inherit = NULL, lock = TRUE, class = TRUE,
                    parent_env = parent.frame()) {

  if (!all_named(public) || !all_named(private) || !all_named(active)) {
    stop("All elements of public, private, and active must be named.")
  }
  if (any(duplicated(c(names(public), names(private), names(active))))) {
    stop("All items in public, private, and active must have unique names.")
  }
  if (any(c("self", "private", "super") %in%
      c(names(public), names(private), names(active)))) {
    stop("Items cannot use reserved names 'self', 'private', and 'super'.")
  }
  if ("initialize" %in% c(names(private), names(active))) {
    stop("'initialize' is not allowed in private or active.")
  }

  if (length(get_nonfunctions(active)) != 0) {
    stop("All items in active must be functions.")
  }

  if (!is.null(inherit)) {
    if (!inherits(inherit, "R6ClassGenerator")) {
      stop("`inherit` must be a R6ClassGenerator.")
    }

    # Merge the new items over the inherited ones
    public  <- merge_vectors(inherit$public,  public)
    private <- merge_vectors(inherit$private, private)
    active  <- merge_vectors(inherit$active,  active)

    # Do some preparation work on the superclass, so that we don't have to do
    # it each time an object is created.
    super_list <- listify_superclass(inherit)
  } else {
    super_list <- NULL
  }

  if (class) {
    classes <- c(classname, get_superclassnames(inherit), "R6")
  } else {
    classes <- NULL
  }

  newfun <- R6Class_newfun(classes, public, private, active, super_list,
                           lock, parent_env)

  structure(
    list(new = newfun, classname = classname, public = public,
         private = private, active = active, inherit = inherit,
         parent_env = parent_env, lock = lock),
    class = "R6ClassGenerator"
  )
}


# Create the $new function for a R6ClassGenerator
R6Class_newfun <- function(classes, public, private, active, super_list,
                           lock, parent_env) {

  has_private <- !is.null(private)

  function(...) {
    if (has_private) {
      private_env <- new.env(parent = parent_env, hash = (length(private) > 100))
      public_env <- new.env(parent = private_env, hash = (length(public) > 100))
    } else {
      public_env <- new.env(parent = parent_env, hash = (length(public) > 100))
    }

    # Fix environment for functions
    public <- assign_func_envs(public, public_env)

    # Copy objects to environments
    list2env2(public, envir = public_env)

    # Add self pointer
    public_env$self <- public_env

    # Do same for private
    if (has_private) {
      private <- assign_func_envs(private, public_env)
      list2env2(private, envir = private_env)
      public_env$private <- private_env
    }

    # Set up active bindings
    if (!is.null(active)) {
      active <- assign_func_envs(active, public_env)

      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], public_env)
      }
    }

    if (!is.null(super_list$functions) || !is.null(super_list$active)) {
      public_env$super <- create_super_env(super_list, public_env)
    }

    if (lock) {
      if (has_private) lockEnvironment(private_env)
      lockEnvironment(public_env)
    }

    class(public_env) <- classes

    if (is.function(public_env$initialize)) {
      public_env$initialize(...)
    } else if (length(list(...)) != 0 ) {
      stop("Called new() with arguments, but there is no initialize method.")
    }
    public_env
  }
}

# Create and populate the self$super environment
create_super_env <- function(super_list, self) {
  functions <- super_list$functions
  active <- super_list$active

  # The environment in which functions evaluate is a child of the enclosing env
  # (should be the self env). Though this is a child of self, self has no
  # bindings that point to it. The only reason this environment is needed is so
  # that if a function super$foo in turn calls super$bar, it will be able to
  # find bar from the next superclass up.
  super_enc_env <- new.env(parent = self, hash = FALSE)

  # The binding environment is a new environment. Its parent doesn't matter
  # because it's not the enclosing environment for any functions.
  super_bind_env <- new.env(parent = emptyenv(),
    hash = length(functions) + length(active) > 100)

  # Set up functions. All the functions can be found in self$super (the binding
  # env). Their enclosing env may or may not be self$super.
  functions <- assign_func_envs(functions, super_enc_env)
  list2env2(functions, envir = super_bind_env)

  # Set up active bindings
  active <- assign_func_envs(active, super_enc_env)
  for (name in names(active)) {
    makeActiveBinding(name, active[[name]], super_bind_env)
  }

  # Recurse if there are more superclasses
  if (!is.null(super_list$super)) {
    super_enc_env$super <- create_super_env(super_list$super, super_enc_env)
  }

  super_bind_env
}

# Given a R6ClassGenerator, recursively convert it into a list that's useful
# for efficiently instantiating $super objects.
listify_superclass <- function(class) {
  if (is.null(class)) return(NULL)

  list(
    functions = c(get_functions(class$public),
                  get_functions(class$private)),
    active = class$active,
    super = listify_superclass(class$inherit)
  )
}
