#' @export
R8Class <- function(classname = NULL, public = list(), private = NULL,
                    active = NULL, inherit = NULL, lock = TRUE, shared = TRUE,
                    class = TRUE, parent_env = parent.frame()) {

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
    if (!inherits(inherit, "R8ClassGenerator")) {
      stop("`inherit` must be a R8ClassGenerator.")
    }

    # Merge the new items over the inherited ones
    public  <- merge_vectors(inherit$public,  public)
    private <- merge_vectors(inherit$private, private)
    active  <- merge_vectors(inherit$active,  active)
  }

  # If methods are shared, extract the methods and put them in an environment
  if (shared) {
    # Separate methods from fields
    public_fields <- get_nonfunctions(public)
    public_methods <- list2env2(get_functions(public))

    private_fields <- get_nonfunctions(private)
    private_methods <- list2env2(get_functions(private))
  }

  if (class) {
    if (shared)
      classes <- c("R8_shared", "R8")
    else
      classes <- "R8"

    classes <- c(classname, get_superclassnames(inherit), classes)

  } else {
    if (!shared)
      stop("R8 classes with shared methods must have class=TRUE")

    classes <- NULL
  }

  newfun <- R8Class_newfun(classes,
                           public_fields, public_methods,
                           private_fields, private_methods,
                           active, inherit, lock, parent_env)


  structure(
    list(
      new = newfun,
      classname = classname,
      public = public,
      private = private,
      public_fields = public_fields,
      public_methods = public_methods,
      private_fields = private_fields,
      private_methods = private_methods,
      active = active,
      inherit = inherit,
      parent_env = parent_env,
      lock = lock
    ),
    class = "R8ClassGenerator"
  )
}


# Create the $new function for a R8ClassGenerator
R8Class_newfun <- function(classes, public_fields, public_methods,
                           private_fields, private_methods, active,
                           inherit, lock, parent_env) {

  function(...) {
    # Create the evaluation environment
    eval_env <- new.env(parent = parent_env, hash = FALSE)

    # Copy public fields to public binding environment
    public_bind_env <- list2env2(public_fields, empty_to_null = FALSE)

    # Add self pointer
    eval_env$self <- public_bind_env

    # Do same for private_fields
    if (!is.null(private_fields) || !is.null(private_methods)) {
      private_bind_env <- list2env2(private_fields, empty_to_null = FALSE)
      eval_env$private <- private_bind_env
      class(private_bind_env) <- "R8_shared"
    }

    # Set up active bindings
    if (!is.null(active)) {
      active <- assign_func_envs(active, eval_env)

      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], public_bind_env)
      }
    }

    if (!is.null(inherit$public_methods) ||
        !is.null(inherit$private_methods) ||
        !is.null(inherit$active)) {
      eval_env$super <- create_r8_super_env(inherit, public_bind_env,
                                            private_bind_env)
    }

    if (lock) {
      lockEnvironment(public_bind_env)
      if (!is.null(private_fields))
        lockEnvironment(private_bind_env)
    }

    # Always lock the eval_env
    lockEnvironment(eval_env)

    class(public_bind_env) <- classes
    attr(public_bind_env, "eval_env") <- eval_env
    attr(public_bind_env, "methods") <- public_methods
    if (!is.null(private_methods)) {
      attr(private_bind_env, "eval_env") <- eval_env
      attr(private_bind_env, "methods") <- private_methods
    }

    if (is.function(public_methods$initialize)) {
      public_bind_env$initialize(...)
    } else if (length(list(...)) != 0 ) {
      stop("Called new() with arguments, but there is no initialize method.")
    }
    public_bind_env
  }
}

# Create a super env
create_r8_super_env <- function(inherit, public_bind_env, private_bind_env) {
  # Unclass inherit for faster access with $ (avoid S3 overhead)
  inherit <- unclass(inherit)

  eval_env <- new.env(parent = inherit$parent_env, hash = FALSE)

  eval_env$self <- public_bind_env
  if (!is.null(private_bind_env))
    eval_env$private <- private_bind_env

  # Set up active bindings
  # The only thing that goes in the binding env are the active bindings.
  active <- inherit$active
  bind_env <- new.env(parent = emptyenv(), hash = length(active) > 100)
  if (!is.null(active)) {
    active <- assign_func_envs(active, eval_env)
    for (name in names(active)) {
      makeActiveBinding(name, active[[name]], bind_env)
    }
  }

  # Recurse if there are more superclasses
  if (!is.null(inherit$inherit)) {
    eval_env$super <- create_r8_super_env(inherit$inherit, public_bind_env,
                                          inherit$parent_env)
  }

  # Always lock the eval_env
  lockEnvironment(eval_env)

  # Add the methods
  attr(bind_env, "methods") <- inherit$public_methods
  attr(bind_env, "methods2") <- inherit$private_methods

  attr(bind_env, "eval_env") <- eval_env
  class(bind_env) <- "R8_shared"
  bind_env
}


#' @export
#' @useDynLib R6 subset_R8
`$.R8_shared` <-  function(x, name) {
  .Call(subset_R8, x, name)
}

#' @export
`[[.R8_shared` <- `$.R8_shared`
