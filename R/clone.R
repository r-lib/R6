# This function will be added as a method to R6 objects, with the name 'clone',
# and with the environment changed.
generator_funs$clone_method <- function(deep = FALSE) {

  # Need to embed these utility functions inside this closure because the
  # environment of this function will change.
  assign_func_envs <- function(objs, target_env) {
    if (is.null(target_env)) return(objs)

    lapply(objs, function(x) {
      if (is.function(x)) environment(x) <- target_env
      x
    })
  }

  list2env2 <- function(x, envir = NULL, parent = emptyenv(),
                        hash = (length(x) >  100),
                        size = max(29L, length(x)),
                        empty_to_null = TRUE) {
    if (is.null(envir)) {
      envir <- new.env(hash = hash, parent = parent, size = size)
    }
    if (length(x) == 0) {
      if (empty_to_null)
        return(NULL)
      else
        return(envir)
    }
    list2env(x, envir)
  }

  clone_super <- function(old_enclos_env, new_enclos_env, public_bind_env,
                          has_private, private_bind_env)
  {
    old_super_bind_env <- old_enclos_env$super
    if (is.null(old_super_bind_env))
      return()

    # Copy all the methods from the old super binding env to the new one, and
    # set their enclosing env to a new one.
    super_copies <- as.list.environment(old_super_bind_env, all.names = TRUE)

    # Degenerate case: super env is empty
    if (length(super_copies) == 0) {
      new_enclos_env$super <- new.env(parent = emptyenv(), hash = FALSE)
      return()
    }

    # All items in the old_super_bind_env must be functions; to get the
    # old_super_enclos_env, simply call environment() on one of them. Doing it
    # this way lets us avoid storing an explicit pointer to the super_enclos_env
    # in the original super_bind_env. This doesn't work as well for avoiding
    # storing the enclos_env in the original public_bind_env, because there are
    # many possible items there. We can't assume that just any item is a
    # function -- and even if we do find a function, it's not guaranteed that
    # it's a method. It may be a function (with a different parent env) that was
    # added after the object was created.
    old_super_enclos_env <- environment(super_copies[[1]])

    # Create new super enclos env and populate with self and private.
    new_super_enclos_env <- new.env(parent = parent.env(old_super_enclos_env),
                                    hash = FALSE)
    new_super_enclos_env$self <- public_bind_env
    if (has_private)
      new_super_enclos_env$private <- private_bind_env

    new_super_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

    # Copy over the methods and fix up their environments
    super_copies <- assign_func_envs(super_copies, new_super_enclos_env)
    list2env2(super_copies, new_super_bind_env)


    new_enclos_env$super <- new_super_bind_env

    # Recurse
    clone_super(old_super_enclos_env, new_super_enclos_env, public_bind_env,
                has_private, private_bind_env)
  }

  # ------------------------------------------------------------------

  old_enclos_env = self$`.__enclos_env__`
  if (!is.environment(old_enclos_env)) {
    stop("clone() must be called from an R6 object.")
  }

  old_public_bind_env <- self
  old_private_bind_env <- old_enclos_env$private
  has_private <- !is.null(old_private_bind_env)

  # Figure out if we're in a portable class object
  portable <- !identical(old_public_bind_env, old_enclos_env)

  # Set up stuff for deep clones
  if (deep) {
    if (has_private && is.function(old_private_bind_env$deep_clone)) {
      # Get private$deep_clone, if available.
      deep_clone <- old_private_bind_env$deep_clone
    } else {
      # If there's no private$deep_clone, then this default function will copy
      # fields that are R6 objects.
      deep_clone <- function(name, value) {
        # Check if it's an R6 object.
        if (is.environment(value) && !is.null(value$`.__enclos_env__`)) {
          return(value$clone(deep = TRUE))
        }
        value
      }
    }
  }

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


  # Copy members ----------------------------------------------------

  # Copy the old objects, fix up method environments, and put them into the
  # new binding environment.
  public_copies <- as.list.environment(old_public_bind_env, all.names = TRUE)
  # Don't copy .__enclose_env__
  public_copies <- public_copies[setdiff(names(public_copies), ".__enclos_env__")]
  public_copies <- assign_func_envs(public_copies, new_enclos_env)

  # Separate active and non-active bindings
  active_idx <- vapply(names(public_copies), bindingIsActive, env = old_public_bind_env,
                       logical(1))
  active_copies <- public_copies[active_idx]
  public_copies <- public_copies[!active_idx]

  if (deep) {
    public_copies <- mapply(deep_clone, names(public_copies), public_copies,
                            SIMPLIFY = FALSE)
  }

  # Copy in public and active bindings
  list2env2(public_copies, public_bind_env)

  if (length(active_copies) > 0) {
    for (name in names(active_copies)) {
      makeActiveBinding(name, active_copies[[name]], public_bind_env)
    }
  }

  # Copy private members
  if (has_private) {
    private_copies <- as.list.environment(old_private_bind_env, all.names = TRUE)
    if (deep) {
      private_copies <- mapply(deep_clone, names(private_copies), private_copies,
                               SIMPLIFY = FALSE)
    }
    private_copies <- assign_func_envs(private_copies, new_enclos_env)
    list2env2(private_copies, private_bind_env)
  }

  # Clone super object -------------------------------------------
  clone_super(old_enclos_env, new_enclos_env, public_bind_env, has_private,
              private_bind_env)

  # Add refs to other environments in the object --------------------
  public_bind_env$`.__enclos_env__` <- new_enclos_env

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

  public_bind_env
}
