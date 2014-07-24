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
#     active  <- merge_vectors(inherit$active,  active)

    # Do some preparation work on the superclass, so that we don't have to do
    # it each time an object is created.
    super_list <- listify_superclass(inherit)
  } else {
    super_list <- NULL
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
      classes <- c("R8shared", "R8")
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
                           active, super_list, lock, parent_env)

  structure(
    list(
      new = newfun,
      classname = classname,
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
                           super_list, lock, parent_env) {

  function(...) {
    # Create the evaluation environment
    eval_env <- new.env(parent = parent_env, hash = FALSE)

    # Copy public fields to public binding environment
    if (is.null(public_fields))
      public_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
    else
      public_bind_env <- list2env2(public_fields)

    # Add self pointer
    eval_env$self <- public_bind_env

    # Do same for private_fields
    if (!is.null(private_fields)) {
      private_bind_env <- list2env2(private_fields)
      eval_env$private <- private_bind_env
    }

    # # Set up active bindings
    # if (!is.null(active)) {
    #   active <- assign_func_envs(active, eval_env)

    #   for (name in names(active)) {
    #     makeActiveBinding(name, active[[name]], public_bind_env)
    #   }
    # }

    # if (!is.null(super_list$functions) || !is.null(super_list$active)) {
    #   eval_env$super <- create_r8_super_env(super_list, public_bind_env, private_bind_env, parent_env)
    # }

    if (lock) {
      lockEnvironment(public_bind_env)
      if (!is.null(private_fields))
        lockEnvironment(private_bind_env)
    }

    # Do locking at end, after adding private and super?
#     lockEnvironment(eval_env)

    class(public_bind_env) <- classes
    attr(public_bind_env, "eval_env") <- eval_env
    attr(public_bind_env, "public_methods") <- public_methods
    if (!is.null(private_methods)) {
      attr(public_bind_env, "private_methods") <- private_methods
    }

    if (is.function(public_methods$initialize)) {
      public_bind_env$initialize(...)
    } else if (length(list(...)) != 0 ) {
      stop("Called new() with arguments, but there is no initialize method.")
    }
    public_bind_env
  }
}

# Create and populate the self$super environment
create_r8_super_env <- function(super_list, public_bind_env, private_bind_env = NULL, parent_env) {
  functions <- super_list$functions
  active <- super_list$active

  # TODO: replace parent_env with the superclass's actual parent environment
  super_eval_env <- new.env(parent = parent_env, hash = FALSE)
  super_bind_env <- new.env(parent = emptyenv(),
                            hash = length(functions) + length(active) > 100)

  super_eval_env$self <- public_bind_env
  if (!is.null(private_bind_env)) {
    super_eval_env$private <- private_bind_env
  }

  # Set up functions. All the functions can be found in self$super (the binding
  # env). Their enclosing env may or may not be self$super.
  if (!is.null(functions)) {
    functions <- assign_func_envs(functions, super_eval_env)
    list2env2(functions, envir = super_bind_env)
  }

  # Set up active bindings
  if (!is.null(active)) {
    active <- assign_func_envs(active, super_enc_env)
    for (name in names(active)) {
      makeActiveBinding(name, active[[name]], super_bind_env)
    }
  }

  # Recurse if there are more superclasses
  if (!is.null(super_list$super)) {
    super_eval_env$super <- create_r8_super_env(super_list$super, public_bind_env, parent_env)
  }

  super_bind_env
}


#' @export
#' @useDynLib R6 subset_R8
`$.R8` <-  function(x, name) {
  .Call(subset_R8, x, name)
}

#' @export
`[[.R8` <- `$.R8`
