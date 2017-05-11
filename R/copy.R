# encl is the objects enclosure assigned to the binding environment of copy at object
# creation time (using new or copy)
generator_funs$copy <- function(deep=FALSE) {
  has_priv <- has_private()

  if (deep) {
    if (!has_priv || is.null(encl$private$deepCopy)) {
      stop('Deep copying only supported for classes with private deepCopy method')
    }
    deep_copies <- encl$private$deepCopy()
  } else {
    deep_copies <- list()
  }

  ## COPIED FROM $new()
  if (portable) {
    # When portable==TRUE, the public binding environment is separate from the
    # enclosing environment.

    # Binding environment for private objects (where private objects are found)
    if (has_priv)
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
    if (has_priv) {
      private_bind_env <- new.env(parent = parent_env, hash = FALSE)
      public_bind_env <- new.env(parent = private_bind_env, hash = FALSE)
    } else {
      private_bind_env <- NULL
      public_bind_env <- new.env(parent = parent_env, hash = FALSE)
    }

    enclos_env <- public_bind_env
  }

  # Handle super enclosure ------------------------------------------
  inherit <- get_inherit()
  if(!is.null(inherit)) {
    if (portable) {
      # Set up the superclass objects
      super_struct <- create_super_env(inherit, public_bind_env,
                                       private_bind_env, portable = TRUE)
    } else {
      # Set up the superclass objects
      super_struct <- create_super_env(inherit, public_bind_env, portable = FALSE)
    }
    enclos_env$super <- super_struct$bind_env
  }

  # Add self and private pointer ------------------------------------
  enclos_env$self <- public_bind_env
  if (has_priv)
    enclos_env$private <- private_bind_env
  ## COPY END

  # Copy private fields and methods, setting new enclosures for methods
  if (has_priv) {
    new_private <- to_list(encl$private)
    new_private_methods <- assign_func_envs(get_functions(new_private), enclos_env)
    new_private_fields <- get_nonfunctions(new_private)
    # Override fields provided by deepCopy()
    if (!is.null(deep_copies$private)) {
      for (i in names(deep_copies$private)) {
        if (!is.null(new_private_fields[[i]])) {
          new_private_fields[[i]] <- deep_copies$private[[i]]
        }
      }
    }
    list2env2(new_private_methods, envir = private_bind_env)
    list2env2(new_private_fields, envir = private_bind_env)
  }

  # Copy public fields and methods, setting new enclosures for methods
  new_public <- to_list(encl$self)
  new_public_methods <- assign_func_envs(get_functions(new_public), enclos_env)
  # Rescue copy enclosure
  new_copy_encl <- list2env2(to_list(parent.env(environment())), parent=parent.env(parent.env(environment())))
  new_copy_encl$encl <- enclos_env
  environment(new_public_methods$copy) <- new_copy_encl

  new_public_fields <- get_nonfunctions(new_public)
  # Override fields provided by deepCopy()
  if (!is.null(deep_copies$public)) {
    for (i in names(deep_copies$public)) {
      if (!is.null(new_public_fields[[i]])) {
        new_public_fields[[i]] <- deep_copies$public[[i]]
      }
    }
  }
  if (!is.null(active)) {
    for (name in names(active)) {
      makeActiveBinding(name, new_public_methods[[name]], public_bind_env)
      new_public_methods[[name]] <- NULL
    }
  }
  list2env2(new_public_methods, envir = public_bind_env)
  list2env2(new_public_fields, envir = public_bind_env)


  # COPIED FROM new()
  # Lock ------------------------------------------------------------
  if (lock) {
    if (has_priv) lockEnvironment(private_bind_env)
    lockEnvironment(public_bind_env)
  }

  # Always lock methods
  if (has_priv) {
    for (name in names(new_private_methods))
      lockBinding(name, private_bind_env)
  }
  for (name in names(new_public_methods))
    lockBinding(name, public_bind_env)

  class(public_bind_env) <- class(encl$self)
  # END COPY

  public_bind_env
}