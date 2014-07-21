#' @export
R8Class <- function(classname = NULL, public = list(),
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

  # Separate methods from non-methods
  fun_idx <- vapply(public, is.function, logical(1))
  public_methods <- public[fun_idx]
  public_fields <- public[!fun_idx]


  # # Enclosing env for methods
  # public_methods <- assign_func_envs(public_methods, parent_env)
  # # Binding env for methods
  # public_methods_env <- new.env(parent = emptyenv(), hash = length(public_methods) > 100)
  # # Turn methods into an environment so that it's possible to add methods later
  # list2env2(public_methods, envir = public_methods_env)
  # if (lock) {
  #   lockEnvironment(public_methods_env)
  # }

  # if (!is.null(inherit)) {
  #   if (!inherits(inherit, "R8ClassGenerator")) {
  #     stop("`inherit` must be a R8ClassGenerator.")
  #   }

  #   # Merge the new items over the inherited ones
  #   public  <- merge_vectors(inherit$public,  public)
  #   private <- merge_vectors(inherit$private, private)
  #   active  <- merge_vectors(inherit$active,  active)

  #   # Do some preparation work on the superclass, so that we don't have to do
  #   # it each time an object is created.
  #   super_list <- listify_superclass(inherit)
  # } else {
  #   super_list <- NULL
  # }

  if (class) {
    classes <- c(classname, get_superclassnames(inherit), "R8")
  } else {
    classes <- NULL
  }

  newfun <- R8Class_newfun(classes, public_fields, public_methods, private, active, super_list,
                           lock, parent_env)

  structure(
    list(new = newfun, classname = classname, public = public,
         private = private, active = active, inherit = inherit,
         parent_env = parent_env, lock = lock),
    class = "R8ClassGenerator"
  )
}


# Create the $new function for a R8ClassGenerator
R8Class_newfun <- function(classes, public_fields, public_methods, private, active, super_list,
                           lock, parent_env) {

  has_private <- !is.null(private)

  function(...) {
    # Create the evaluation environment
    eval_env <- new.env(parent = parent_env, hash = FALSE)

    # Create the binding environment
    public_bind_env <- new.env(parent = emptyenv(),
                               hash = (length(public_fields) > 100))
    # Add self pointer
    eval_env$self <- public_bind_env

    # # Fix environment for functions
    # public <- assign_func_envs(public, eval_env)

    # Copy objects to environments
    list2env2(public_fields, envir = public_bind_env)

    # # Do same for private
    # if (has_private) {
    #   private_bind_env <- new.env(parent = emptyenv(),
    #                               hash = (length(private) > 100))
    #   eval_env$private <- private_bind_env
    #   private <- assign_func_envs(private, eval_env)
    #   list2env2(private, envir = private_bind_env)
    # }

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

    # if (lock) {
    #   if (has_private) lockEnvironment(private_bind_env)
    #   lockEnvironment(public_bind_env)
    # }

    # Do locking at end, after adding private and super?
#     lockEnvironment(eval_env)

    class(public_bind_env) <- classes

    attr(public_bind_env, "public_methods") <- public_methods
    attr(public_bind_env, "eval_env") <- eval_env

    if (is.function(public_bind_env$initialize)) {
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
`$.R8` <- function(x, name) {
  methods <- attr(x, "public_methods", exact = TRUE)
  fun <- methods[[name]]

  if (!is.null(fun)) {
    eval_env <- attr(x, "eval_env", exact = TRUE)
    environment(fun) <- eval_env
    fun

  } else {
    .subset2(x, name)
  }
}

#' @export
`[[.R8` <- `$.R8`
