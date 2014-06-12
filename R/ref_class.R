#' Create a reference class
#'
#' Classes created by this generator have the following properties:
#' \itemize{
#'   \item Has public members, and optionally has private members, as well as
#'     active bindings.
#'   \item If there are any private members, they are put in a private
#'     environment, which is the parent of the public environment. The parent
#'     of the private environment is set with the \code{parent_env} argument.
#'   \item If there are no private members, then no private environment is
#'     created, and the parent of the public environment is set with
#'     \code{parent_env}.
#'   \item If present, active bindings are put in the public environment.
#'   \item The generator's \code{$new} method creates a new object and returns
#'     its public environment, which has a class attribute.
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
#' semantics, those items will be shared across all instances of the class.
#' To avoid this, add an entry for that item with a \code{NULL} initial value,
#' and then in the \code{intialize} method, instantiate the object and assign
#' it.
#'
#' Normally the public environment will have two classes: the one supplied in
#' the \code{classname} argument, and \code{"RefClass"}. It is possible to
#' get the public environment with no classes, by using \code{class=FALSE}.
#' This will result in faster access speeds by avoiding class-based dispatch
#' of \code{$}. The benefit is is negligible in most cases. With classes,
#' accessing a member with \code{$} takes around 2 microseconds on a modern
#' machine; without classes, it takes around 0.3 microseconds. This will make
#' a noticeable difference in performance only when there are hundreds of
#' thousands or more iterations.
#'
#' The primary difference in behavior when \code{class=FALSE} is that pretty
#' printing of the objects (with \code{print.RefClass}) won't be used.
#'
#' @seealso \code{\link{makeActiveBinding}}
#' @export
#' @param classname Name of the class.
#' @param public A list of public members, which can be functions and
#'   non-functions.
#' @param private An optional list of private members, which can be functions
#'   and non-functions.
#' @param active An optional list of active binding functions.
#' @param inherit A RefClassGenerator object to inherit from; in other words,
#'   a superclass for this class.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param class Should a class attribute be added to the public environment?
#'   Default is \code{TRUE}.
#' @param lock Should the environments of the generated objects be locked?
#' @examples
#' # A simple class
#' AnimalHerd <- createRefClass("AnimalHerd",
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
#' MyClass <- createRefClass("MyClass",
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
#' # Print, using the print.RefClass method:
#' print(z)
#'
#'
#'
#' # Inheritance
#'
createRefClass <- function(classname = NULL, public = list(),
                           private = NULL, active = NULL,
                           inherit = NULL, parent_env = parent.frame(),
                           lock = TRUE, class = TRUE) {

  if (!all_named(public) || !all_named(private) || !all_named(active)) {
    stop("All elements of public, private, and active must be named.")
  }
  if (any(duplicated(c(names(public), names(private), names(active))))) {
    stop("All items in public, private, and active must have unique names.")
  }
  if (length(get_nonfunctions(active)) != 0) {
    stop("All items in active must be functions.")
  }

  if (!is.null(inherit)) {
    if (!inherits(inherit, "RefClassGenerator")) {
      stop("`inherit` must be a RefClass.")
    }

    # Merge the new items over the inherited ones
    public  <- merge_vectors(inherit$public,  public)
    private <- merge_vectors(inherit$private, private)
    active  <- merge_vectors(inherit$active,  active)

    # Get just the functions from the inherited class.
    # These will be placed in the super env.
    super_funs <- c(get_functions(inherit$public), get_functions(inherit$private))
    super_active <- inherit$active
  }

  has_private <- !is.null(private)

  newfun <- function(...) {
    if (has_private) {
      private_env <- new.env(parent = parent_env, hash = (length(private) > 100))
      public_env <- new.env(parent = private_env, hash = (length(public) > 100))
    } else {
      public_env <- new.env(parent = parent_env, hash = (length(public) > 100))
    }

    # Fix environment for functions
    public <- assign_func_envs(public, public_env)

    # Copy objects to environments
    list2env(public, envir = public_env)

    # Add self pointer
    public_env$self <- public_env

    # Do same for private
    if (has_private) {
      private <- assign_func_envs(private, public_env)
      list2env(private, envir = private_env)
      public_env$private <- private_env
    }

    # Set up active bindings
    if (!is.null(active)) {
      active <- assign_func_envs(active, public_env)

      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], public_env)
      }
    }

    # Set up super environment, which contains just the functions from the
    # inherited class.
    if (!is.null(inherit) &&
        (!is.null(super_funs) || !is.null(super_active))) {
      super_env <- new.env(parent = emptyenv(),
        hash = length(super_funs) + length(super_active) > 100)

      if (!is.null(super_funs)) {
        super_funs <- assign_func_envs(super_funs, public_env)
        list2env(super_funs, envir = super_env)
      }

      if (!is.null(super_active)) {
        super_active <- assign_func_envs(super_active, public_env)
        for (name in names(super_active)) {
          makeActiveBinding(name, super_active[[name]], super_env)
        }
      }

      public_env$super <- super_env
    }

    if (lock) {
      if (has_private) lockEnvironment(private_env)
      lockEnvironment(public_env)
    }
    if (is.function(public_env$initialize)) public_env$initialize(...)

    if (class) class(public_env) <- c(classname, "RefClass")
    public_env
  }

  structure(
    list(new = newfun, classname = classname, public = public,
         private = private, active = active, inherit = inherit,
         parent_env = parent_env, lock = lock),
    class = "RefClassGenerator"
  )
}

get_functions <- function(x) {
  funcs <- vapply(x, is.function, logical(1))
  if (all(!funcs)) return(NULL)
  x[funcs]
}

get_nonfunctions <- function(x) {
  funcs <- vapply(x, is.function, logical(1))
  if (all(funcs)) return(NULL)
  x[!funcs]
}
