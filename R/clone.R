#' Clone an R6 object
#'
#' @param obj An R6 object to clone.
#'
#' @export
clone <- encapsulate(function(obj) {
  old_enclos_env <- attr(obj, "enclos_env", TRUE)

  if (!is.environment(old_enclos_env)) {
    stop("`obj` must be an R6 object.")
  }

  old_public_bind_env <- old_enclos_env$self
  old_private_bind_env <- old_enclos_env$private
  has_private <- !is.null(old_private_bind_env)

  # Figure out if we're in a portable class object
  portable <- !identical(old_public_bind_env, old_enclos_env)

  # Create the new binding and enclosing environments
  if (portable) {
    if (has_private) {
      private_bind_env <- new.env(emptyenv(), hash = FALSE)
    }
    public_bind_env <- new.env(emptyenv(), hash = FALSE)
    new_enclos_env <- new.env(parent.env(old_enclos_env), hash = FALSE)

  } else {
    if (has_private) {
      private_bind_env <- new.env(parent.env(old_private_bind_env), hash = FALSE)
      public_bind_env <- new.env(private_bind_env, hash = FALSE)
    } else {
      public_bind_env <- new.env(parent.env(old_public_bind_env), hash = FALSE)
    }
    new_enclos_env <- public_bind_env
  }

  # Copy the old objects, fix up method environments, and put them into the
  # new binding environment.
  public_copies <- as.list.environment(old_public_bind_env)
  public_copies <- assign_func_envs(public_copies, new_enclos_env)
  list2env2(public_copies, public_bind_env)

  if (has_private) {
    private_copies <- as.list.environment(old_private_bind_env)
    private_copies <- assign_func_envs(private_copies, new_enclos_env)
    list2env2(private_copies, private_bind_env)
  }

  # Lock --------------------------------------------------------------
  # Copy locked state of environment
  if (environmentIsLocked(old_public_bind_env)) {
    lockEnvironment(public_bind_env)
  }
  if (has_private && environmentIsLocked(old_private_bind_env)) {
    lockEnvironment(private_bind_env)
  }

  # Always lock methods
  # We inspect the names in public_copies instead public_bind_env, because
  # ls() is so slow for environments. R 3.2.0 introduced the sorted=FALSE
  # option, which makes ls() much faster, so at some point we'll be able to
  # switch to that.
  for (name in names(public_copies)) {
    if (is.function(public_bind_env[[name]]))
      lockBinding(name, public_bind_env)
  }
  if (has_private) {
    for (name in names(private_copies)) {
      if (is.function(private_bind_env[[name]]))
        lockBinding(name, private_bind_env)
    }
  }

  # Add self and (optional) private pointer ---------------------------
  new_enclos_env$self <- public_bind_env
  if (has_private)
    new_enclos_env$private <- private_bind_env

  class(public_bind_env) <- class(old_public_bind_env)

  attr(public_bind_env, "enclos_env") <- new_enclos_env
  public_bind_env
})

