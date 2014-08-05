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
#' @section The \code{print} method:
#'
#' R6 object generators and R6 objects have a default \code{print} method
#' to show them on the screen: they simply list the members and parameters
#' (e.g. lock, portable, etc., see above) of the object.
#'
#' The default \code{print} method of R6 objects can be redefined,
#' by supplying a public \code{print} method. (\code{print} members that
#' are not functions are ignored.) This method is automatically called
#' whenever the object is printed, e.g. when the object's name is typed
#' at the command prompt, or when \code{print(obj)} is called. It can also
#' be called directly via \code{obj$print()}. All extra arguments from a
#' \code{print(obj, ...)} call are passed on to the \code{obj$print(...)}
#' method.
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
#' @param portable If \code{TRUE}, this class will work with inheritance across
#'   different packages. Note that when this is enabled, fields and members must
#'   be accessed with  \code{self$x} or \code{private$x}; they can't be
#'   accessed with just \code{x}.
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
                    portable = FALSE,
                    parent_env = parent.frame()) {

  if (!all_named(public) || !all_named(private) || !all_named(active))
    stop("All elements of public, private, and active must be named.")

  if (any(duplicated(c(names(public), names(private), names(active)))))
    stop("All items in public, private, and active must have unique names.")

  if (any(c("self", "private", "super") %in%
      c(names(public), names(private), names(active))))
    stop("Items cannot use reserved names 'self', 'private', and 'super'.")

  if ("initialize" %in% c(names(private), names(active)))
    stop("'initialize' is not allowed in private or active.")

  if (length(get_nonfunctions(active)) != 0)
    stop("All items in active must be functions.")

  if (!is.null(inherit)) {
    if (!inherits(inherit, "R6ClassGenerator"))
      stop("`inherit` must be a R6ClassGenerator.")

    if (!identical(portable, inherit$portable))
      stop("Sub and superclass must both be portable or non-portable.")

    # Do some preparation work on the superclass, so that we don't have to do
    # it each time an object is created.
    super <- listify_superclass(
      inherit,
      list(public = public, private = private, active = active)
    )
  } else {
    super <- NULL
  }

  if (class) {
    classes <- c(classname, get_superclassnames(inherit), "R6")
  } else {
    classes <- NULL
  }

  newfun <- R6_newfun(classes, public, private, active, super,
                      lock, portable, parent_env)

  structure(
    list(new = newfun, classname = classname, public = public,
         private = private, active = active,
         inherit = inherit,
         super = super,
         portable = portable, parent_env = parent_env, lock = lock),
    class = "R6ClassGenerator"
  )
}


# Create the $new function for a R6ClassGenerator
R6_newfun <- function(classes, public, private, active, super,
                      lock, portable, parent_env) {

  has_private <- !is.null(private)

  function(...) {
    # Create binding and enclosing environments -----------------------
    if (portable) {
      # When portable==TRUE, the public binding environment is separate from the
      # enclosing environment.

      # Binding environment for private objects (where private objects are found)
      if (has_private)
        private_bind_env <- new.env(parent = emptyenv(), hash = length(private) > 100)
      else
        private_bind_env <- NULL

      # Binding environment for public objects (where public objects are found)
      public_bind_env <- new.env(parent = emptyenv(), hash = length(public) > 100)

      # The enclosing environment for methods
      enclos_env <- new.env(parent = parent_env, hash = FALSE)

    } else {
      # When portable==FALSE, the public binding environment is the same as the
      # enclosing environment.
      # If present, the private binding env is the parent of the public binding
      # env.
      if (has_private) {
        private_bind_env <- new.env(parent = parent_env, hash = (length(private) > 100))
        public_bind_env <- new.env(parent = private_bind_env, hash = (length(public) > 100))
      } else {
        private_bind_env <- NULL
        public_bind_env <- new.env(parent = parent_env, hash = (length(public) > 100))
      }

      enclos_env <- public_bind_env
    }

    # Set up public objects -------------------------------------------
    # Fix environment for methods
    public <- assign_func_envs(public, enclos_env)
    # Copy objects to environments
    list2env2(public, envir = public_bind_env)
    # Add self pointer
    enclos_env$self <- public_bind_env

    # Set up private objects ------------------------------------------
    if (has_private) {
      private <- assign_func_envs(private, enclos_env)
      list2env2(private, envir = private_bind_env)
      enclos_env$private <- private_bind_env
    }

    # Set up active bindings ------------------------------------------
    if (!is.null(active)) {
      active <- assign_func_envs(active, enclos_env)

      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], public_bind_env)
      }
    }

    # Set up superclass objects ---------------------------------------
    if (!is.null(super)) {
      if (portable) {
        enclos_env$super <- create_portable_super_env(super, public_bind_env, private_bind_env)

      } else {
        enclos_env$super <- create_super_env(super, public_bind_env, private_bind_env)
      }
    }


    if (lock) {
      if (has_private) lockEnvironment(private_bind_env)
      lockEnvironment(public_bind_env)
    }

    class(public_bind_env) <- classes

    # Initialize ------------------------------------------------------
    if (is.function(public_bind_env$initialize)) {
      public_bind_env$initialize(...)
    } else if (length(list(...)) != 0 ) {
      stop("Called new() with arguments, but there is no initialize method.")
    }
    public_bind_env
  }
}


# Create and populate the self$super environment, for non-portable case
create_super_env <- function(super, public_bind_env, private_bind_env = NULL) {
  functions <- super$functions
  active <- super$active

  # The environment in which functions evaluate is a child of the enclosing env
  # (should be the self env). Though this is a child of self, self has no
  # bindings that point to it. The only reason this environment is needed is so
  # that if a function super$foo in turn calls super$bar, it will be able to
  # find bar from the next superclass up.
  super_enclos_env <- new.env(parent = public_bind_env, hash = FALSE)

  # The binding environment is a new environment. Its parent doesn't matter
  # because it's not the enclosing environment for any functions.
  super_bind_env <- new.env(parent = emptyenv(),
                            hash = length(functions) + length(active) > 100)

  # Set up functions. All the functions can be found in self$super (the binding
  # env). Their enclosing env may or may not be self$super.
  functions <- assign_func_envs(functions, super_enclos_env)
  list2env2(functions, envir = super_bind_env)

  # Set up active bindings
  active <- assign_func_envs(active, super_enclos_env)
  for (name in names(active)) {
    makeActiveBinding(name, active[[name]], super_bind_env)
  }

  # Copy the non-masked objects from super$public, $private, $active to the
  # subclass's public and private bind env.
  inherit_super_funs(super, super_enclos_env, public_bind_env, private_bind_env)


  # Recurse if there are more superclasses
  if (!is.null(super$super)) {
    super_enclos_env$super <- create_super_env(super$super, public_bind_env,
                                               private_bind_env)
  }

  super_bind_env
}


# Create and populate the self$super environment, for portable case
create_portable_super_env <- function(super, public_bind_env, private_bind_env = NULL) {
  functions <- super$functions
  active <- super$active

  super_enclos_env <- new.env(parent = super$parent_env, hash = FALSE)
  super_bind_env <- new.env(parent = emptyenv(),
                            hash = length(functions) + length(active) > 100)

  super_enclos_env$self <- public_bind_env
  if (!is.null(private_bind_env)) {
    super_enclos_env$private <- private_bind_env
  }

  # Set up functions. All the functions can be found in self$super (the binding
  # env). Their enclosing env may or may not be self$super.
  functions <- assign_func_envs(functions, super_enclos_env)
  list2env2(functions, envir = super_bind_env)

  # Set up active bindings
  active <- assign_func_envs(active, super_enclos_env)
  for (name in names(active)) {
    makeActiveBinding(name, active[[name]], super_bind_env)
  }

  # Recurse if there are more superclasses
  if (!is.null(super$super)) {
    super_enclos_env$super <- create_portable_super_env(super$super,
                                  public_bind_env, private_bind_env)
  }

  super_bind_env
}


# Given a R6ClassGenerator, recursively convert it into a list that's useful
# for efficiently instantiating $super objects.
#
# @param super An R6ClassGenerator object for the superclass.
# @param sub A list containing lists of public, private, and active objects in
#   the subclass. This is used to decide which of the functions from the
#   superclass should be copied to the subclass's binding environment (while
#   maintaining the superclass's enclosing environment). The reason that this is
#   a list and not a full R6ClassGenerator object is because the object hasn't
#   necessarily been created when this function is called.
listify_superclass <- function(super, sub) {
  if (is.null(super)) return(NULL)

  list(
    functions = c(get_functions(super$public), get_functions(super$private)),
    active = super$active,
    nonmasked_public = names_setdiff(get_functions(super$public),
                                     get_functions(sub$public)),
    nonmasked_private = names_setdiff(get_functions(super$private),
                                      get_functions(sub$private)),
    nonmasked_active = names_setdiff(get_functions(super$active),
                                     get_functions(sub$active)),
    parent_env = super$parent_env,
    super = listify_superclass(super$inherit, super)
  )
}

# Populate a public_bind_env and private_bind_env from a super-list obj, and 
# set the enclosing environment for the functions to super_enclos_env.
# This function is used for its side-effects of modifying the public_bind_env
# and private_bind_env.
inherit_super_funs <- function(super, super_enclos_env, public_bind_env,
                               private_bind_env) {
  public <- super$nonmasked_public
  private <- super$nonmasked_private
  active <- super$nonmasked_active

  # Set up public objects -------------------------------------------
  public <- assign_func_envs(super$nonmasked_public, super_enclos_env)
  list2env2(public, envir = public_bind_env)

  # Set up private objects ------------------------------------------
  if (length(private) != 0) {
    private <- assign_func_envs(private, super_enclos_env)
    list2env2(private, envir = private_bind_env)
  }

  # Set up active bindings ------------------------------------------
  if (length(active) != 0) {
    active <- assign_func_envs(active, super_enclos_env)
    for (name in names(active)) {
      makeActiveBinding(name, active[[name]], public_bind_env)
    }
  }
}
