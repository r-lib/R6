#' Create an R6 reference object generator
#'
#' R6 objects are essentially environments, structured in a way that makes them
#' look like an object in a more typical object-oriented language than R. They
#' support public and private members, as well as inheritance across different
#' packages.
#'
#' The \code{active} argument is a list of active binding functions. These
#' functions take one argument. They look like regular variables, but when
#' accessed, a function is called with an optional argument. For example, if
#' \code{obj$x2} is an active binding, then when accessed as \code{obj$x2}, it
#' calls the \code{x2()} function that was in the \code{active} list, with no
#' arguments. However, if a value is assigned to it, as in \code{obj$x2 <- 50},
#' then the function is called with the right-side value as its argument, as in
#' \code{x2(50)}.
#'
#' If the public or private lists contain any items that have reference
#' semantics (for example, an environment), those items will be shared across
#' all instances of the class. To avoid this, add an entry for that item with a
#' \code{NULL} initial value, and then in the \code{intialize} method,
#' instantiate the object and assign it.
#'
#' @section The \code{print} method:
#'
#'   R6 object generators and R6 objects have a default \code{print} method to
#'   show them on the screen: they simply list the members and parameters (e.g.
#'   lock, portable, etc., see above) of the object.
#'
#'   The default \code{print} method of R6 objects can be redefined, by
#'   supplying a public \code{print} method. (\code{print} members that are not
#'   functions are ignored.) This method is automatically called whenever the
#'   object is printed, e.g. when the object's name is typed at the command
#'   prompt, or when \code{print(obj)} is called. It can also be called directly
#'   via \code{obj$print()}. All extra arguments from a \code{print(obj, ...)}
#'   call are passed on to the \code{obj$print(...)} method.
#'
#' @section Portable and non-portable classes:
#'
#'   When R6 classes are portable (the default), they can be inherited across
#'   packages without complication. However, when in portable mode, members must
#'   be accessed with \code{self} and \code{private}, as in \code{self$x} and
#'   \code{private$y}.
#'
#'   When used in non-portable mode, R6 classes behave more like reference
#'   classes: inheritance across packages will not work well, and \code{self}
#'   and \code{private} are not necessary for accessing fields.
#'
#' @section S3 details:
#'
#'   Normally the public environment will have two classes: the one supplied in
#'   the \code{classname} argument, and \code{"R6Class"}. It is possible to get
#'   the public environment with no classes, by using \code{class = FALSE}. This
#'   will result in faster access speeds by avoiding class-based dispatch of
#'   \code{$}. The benefit is is negligible in most cases. With classes,
#'   accessing a member with \code{$} takes around 2 microseconds on a modern
#'   machine; without classes, it takes around 0.3 microseconds. This will make
#'   a noticeable difference in performance only when there are hundreds of
#'   thousands or more iterations.
#'
#'   The primary difference in behavior when \code{class=FALSE} is that, without
#'   a class attribute, it won't be possible to use S3 methods with the objects,
#'   and so pretty printing (with \code{print.R6Class}) won't be used.
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
#' @param inherit A R6ClassGenerator object to inherit from; in other words, a
#'   superclass.
#' @param portable If \code{TRUE} (the default), this class will work with
#'   inheritance across different packages. Note that when this is enabled,
#'   fields and members must be accessed with  \code{self$x} or
#'   \code{private$x}; they can't be accessed with just \code{x}.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param class Should a class attribute be added to the public environment?
#'   Default is \code{TRUE}.
#' @param lock Should the environments of the generated objects be locked? If
#'   lcoked, new members can't be added to the objects.
#' @examples
#' # A queue ---------------------------------------------------------
#' Queue <- R6Class("Queue",
#'   public = list(
#'     initialize = function(...) {
#'       for (item in list(...)) {
#'         self$add(item)
#'       }
#'     },
#'     add = function(x) {
#'       private$queue <- c(private$queue, list(x))
#'       invisible(self)
#'     },
#'     remove = function() {
#'       if (private$length() == 0) return(NULL)
#'       # Can use private$queue for explicit access
#'       head <- private$queue[[1]]
#'       private$queue <- private$queue[-1]
#'       head
#'     }
#'   ),
#'   private = list(
#'     queue = list(),
#'     length = function() base::length(private$queue)
#'   )
#' )
#'
#' q <- Queue$new(5, 6, "foo")
#'
#' # Add and remove items
#' q$add("something")
#' q$add("another thing")
#' q$add(17)
#' q$remove()
#' #> [1] 5
#' q$remove()
#' #> [1] 6
#'
#' # Private members can't be accessed directly
#' q$queue
#' #> NULL
#' # q$length()
#' #> Error: attempt to apply non-function
#'
#' # add() returns self, so it can be chained
#' q$add(10)$add(11)$add(12)
#'
#' # remove() returns the value removed, so it's not chainable
#' q$remove()
#' #> [1] "foo"
#' q$remove()
#' #> [1] "something"
#' q$remove()
#' #> [1] "another thing"
#' q$remove()
#' #> [1] 17
#'
#'
#' # Active bindings -------------------------------------------------
#' Numbers <- R6Class("Numbers",
#'   public = list(
#'     x = 100
#'   ),
#'   active = list(
#'     x2 = function(value) {
#'       if (missing(value)) return(self$x * 2)
#'       else self$x <- value/2
#'     },
#'     rand = function() rnorm(1)
#'   )
#' )
#'
#' n <- Numbers$new()
#' n$x
#' #> [1] 100
#' n$x2
#' #> [1] 200
#' n$x2 <- 1000
#' n$x
#' #> [1] 500
#'
#' # If the function takes no arguments, it's not possible to use it with <-:
#' n$rand
#' #> [1] 0.2648
#' n$rand
#' #> [1] 2.171
#' # n$rand <- 3
#' #> Error: unused argument (quote(3))
#'
#'
#' # Inheritance -----------------------------------------------------
#' # Note that this isn't very efficient - it's just for illustrating inheritance.
#' HistoryQueue <- R6Class("HistoryQueue",
#'   inherit = Queue,
#'   public = list(
#'     show = function() {
#'       cat("Next item is at index", private$head_idx + 1, "\n")
#'       for (i in seq_along(private$queue)) {
#'         cat(i, ": ", private$queue[[i]], "\n", sep = "")
#'       }
#'     },
#'     remove = function() {
#'       if (private$length() - private$head_idx == 0) return(NULL)
#'       private$head_idx <<- private$head_idx + 1
#'       private$queue[[private$head_idx]]
#'     }
#'   ),
#'   private = list(
#'     head_idx = 0
#'   )
#' )
#'
#' hq <- HistoryQueue$new(5, 6, "foo")
#' hq$show()
#' #> Next item is at index 1
#' #> 1: 5
#' #> 2: 6
#' #> 3: foo
#' hq$remove()
#' #> [1] 5
#' hq$show()
#' #> Next item is at index 2
#' #> 1: 5
#' #> 2: 6
#' #> 3: foo
#' hq$remove()
#' #> [1] 6
#'
#'
#'
#' # Calling superclass methods with super$ --------------------------
#' CountingQueue <- R6Class("CountingQueue",
#'   inherit = Queue,
#'   public = list(
#'     add = function(x) {
#'       private$total <<- private$total + 1
#'       super$add(x)
#'     },
#'     get_total = function() private$total
#'   ),
#'   private = list(
#'     total = 0
#'   )
#' )
#'
#' cq <- CountingQueue$new("x", "y")
#' cq$get_total()
#' #> [1] 2
#' cq$add("z")
#' cq$remove()
#' #> [1] "x"
#' cq$remove()
#' #> [1] "y"
#' cq$get_total()
#' #> [1] 3
#'
#'
#' # Non-portable classes --------------------------------------------
#' # By default, R6 classes are portable, which means they can be inherited
#' # across different packages. Portable classes require using self$ and
#' # private$ to access members.
#' # When used in non-portable mode, members can be accessed without self$,
#' # and assignments can be made with <<-.
#'
#' NP <- R6Class("NP",
#'   portable = FALSE,
#'   public = list(
#'     x = NA,
#'     getx = function() x,
#'     setx = function(value) x <<- value
#'   )
#' )
#'
#' np <- NP$new()
#' np$setx(10)
#' np$getx()
#' #> [1] 10
R6Class <- function(classname = NULL, public = list(),
                    private = NULL, active = NULL,
                    inherit = NULL, lock = TRUE, class = TRUE,
                    portable = TRUE,
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

  # Separate fields from methods
  public_fields <- get_nonfunctions(public)
  private_fields <- get_nonfunctions(private)
  public_methods <- get_functions(public)
  private_methods <- get_functions(private)

  # Capture the unevaluated expression for the superclass; when evaluated in
  # the parent_env, it should return the superclass object.
  inherit <- substitute(inherit)

  # This function returns the superclass object
  get_inherit <- function() {
    # The baseenv() arg speeds up eval a tiny bit
    eval(inherit, parent_env, baseenv())
  }

  newfun <- R6_newfun(classname, public_fields, public_methods,
                      private_fields, private_methods, active,
                      get_inherit, lock, portable, parent_env, class)

  structure(
    list(
      new = newfun,
      classname = classname,
      public_fields = public_fields,
      private_fields = private_fields,
      public_methods = public_methods,
      private_methods = private_methods,
      active = active,
      inherit = inherit,
      get_inherit = get_inherit,
      portable = portable,
      parent_env = parent_env,
      lock = lock
    ),
    class = "R6ClassGenerator"
  )
}


# Create the $new function for a R6ClassGenerator
R6_newfun <- function(classname, public_fields, public_methods,
                      private_fields, private_methods, active,
                      get_inherit, lock, portable, parent_env, class) {

  function(...) {
    # Get superclass object -------------------------------------------
    inherit <- get_inherit()

    # Some checks on superclass ---------------------------------------
    if (!is.null(inherit)) {
      if (!inherits(inherit, "R6ClassGenerator"))
        stop("`inherit` must be a R6ClassGenerator.")

      if (!identical(portable, inherit$portable))
        stop("Sub and superclass must both be portable or non-portable.")
    }

    if (class) {
      classes <- c(classname, get_superclassnames(inherit), "R6")
    } else {
      classes <- NULL
    }

    # Precompute some things ------------------------------------------
    has_private <- !(is.null(private_fields) && is.null(private_methods))

    # Create binding and enclosing environments -----------------------
    if (portable) {
      # When portable==TRUE, the public binding environment is separate from the
      # enclosing environment.

      # Binding environment for private objects (where private objects are found)
      if (has_private)
        private_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
      else
        private_bind_env <- NULL

      # Binding environment for public objects (where public objects are found)
      public_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

      # The enclosing environment for methods
      enclos_env <- new.env(parent = parent_env, hash = FALSE)

    } else {
      # When portable==FALSE, the public binding environment is the same as the
      # enclosing environment.
      # If present, the private binding env is the parent of the public binding
      # env.
      if (has_private) {
        private_bind_env <- new.env(parent = parent_env, hash = FALSE)
        public_bind_env <- new.env(parent = private_bind_env, hash = FALSE)
      } else {
        private_bind_env <- NULL
        public_bind_env <- new.env(parent = parent_env, hash = FALSE)
      }

      enclos_env <- public_bind_env
    }

    # Add self and private pointer ------------------------------------
    enclos_env$self <- public_bind_env
    if (has_private)
      enclos_env$private <- private_bind_env

    # Fix environment for methods -------------------------------------
    public_methods <- assign_func_envs(public_methods, enclos_env)
    if (has_private)
      private_methods <- assign_func_envs(private_methods, enclos_env)
    if (!is.null(active))
      active <- assign_func_envs(active, enclos_env)


    # Set up superclass objects ---------------------------------------
    if (!is.null(inherit)) {
      if (portable) {
        # Set up the superclass objects
        super_struct <- create_super_env(inherit, public_bind_env,
                                         private_bind_env, portable = TRUE)
      } else {
        # Set up the superclass objects
        super_struct <- create_super_env(inherit, public_bind_env, portable = FALSE)
      }

      enclos_env$super <- super_struct$bind_env

      # Merge this level's methods over the superclass methods
      public_methods  <- merge_vectors(super_struct$public_methods, public_methods)
      private_methods <- merge_vectors(super_struct$private_methods, private_methods)
      active          <- merge_vectors(super_struct$active, active)


      # Merge fields over superclass fields, recursively --------------
      recursive_merge <- function(obj, which) {
        if (is.null(obj)) return(NULL)
        merge_vectors(recursive_merge(obj$get_inherit(), which), obj[[which]])
      }
      public_fields  <- merge_vectors(recursive_merge(inherit, "public_fields"),
                                      public_fields)
      private_fields <- merge_vectors(recursive_merge(inherit, "private_fields"),
                                      private_fields)
    }

    # Copy objects to public bind environment -------------------------
    list2env2(public_methods, envir = public_bind_env)
    list2env2(public_fields, envir = public_bind_env)

    # Copy objects to private bind environment ------------------------
    if (has_private) {
      list2env2(private_methods, envir = private_bind_env)
      list2env2(private_fields, envir = private_bind_env)
    }

    # Set up active bindings ------------------------------------------
    if (!is.null(active)) {
      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], public_bind_env)
      }
    }

    # Lock ------------------------------------------------------------
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


# Create and populate the self$super environment, for non-portable case.
# In this function, we "climb to the top" of the superclass hierarchy by
# recursing early on in the function, and then fill the methods downward by
# doing the work for each level and passing the needed information down.
create_super_env <- function(inherit, public_bind_env, private_bind_env = NULL,
                             portable = TRUE) {
  public_methods  <- inherit$public_methods
  private_methods <- inherit$private_methods
  active          <- inherit$active

  # Set up super enclosing and binding environments -------------------

  # The environment in which functions run is a child of the public bind env
  # (AKA self).
  # For portable classes, this is a child of the superclass's parent env.
  # For non-portable classes, this is a child of self; however, self has no
  # bindings that point to it. The only reason this environment is needed is so
  # that if a function super$foo in turn calls super$bar, it will be able to
  # find bar from the next superclass up.
  if (portable)
    enclos_parent <- inherit$parent_env
  else
    enclos_parent <- public_bind_env

  super_enclos_env <- new.env(parent = enclos_parent, hash = FALSE)

  # The binding environment is a new environment. Its parent doesn't matter
  # because it's not the enclosing environment for any functions.
  super_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

  # Add self/private pointers -----------------------------------------
  if (portable) {
    super_enclos_env$self <- public_bind_env
    if (!is.null(private_bind_env))
      super_enclos_env$private <- private_bind_env
  }

  # Set up method environments ----------------------------------------
  # All the methods can be found in self$super (the binding env).
  # Their enclosing env is a different environment.
  public_methods  <- assign_func_envs(public_methods, super_enclos_env)
  private_methods <- assign_func_envs(private_methods, super_enclos_env)
  active          <- assign_func_envs(active, super_enclos_env)

  # Recurse if there are more superclasses ----------------------------
  inherit_inherit <- inherit$get_inherit()
  if (!is.null(inherit_inherit)) {
    super_struct <- create_super_env(inherit_inherit, public_bind_env,
                                     private_bind_env, portable)
    super_enclos_env$super <- super_struct$bind_env

    # Merge this level's methods over the superclass methods
    public_methods  <- merge_vectors(super_struct$public_methods, public_methods)
    private_methods <- merge_vectors(super_struct$private_methods, private_methods)
    active          <- merge_vectors(super_struct$active, active)
  }

  # Copy the methods into the binding environment ---------------------
  list2env2(public_methods, envir = super_bind_env)
  list2env2(private_methods, envir = super_bind_env)
  for (name in names(active)) {
    makeActiveBinding(name, active[[name]], super_bind_env)
  }

  # Return an object with all the information needed to merge down
  list(
    bind_env = super_bind_env,
    public_methods = public_methods,
    private_methods = private_methods,
    active = active
  )
}
