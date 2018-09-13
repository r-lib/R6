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

  # ---------------------------------------------------------------------------
  # Create representation of the old object
  # ---------------------------------------------------------------------------
  old <- list(
    list(
      enclosing = .subset2(self, ".__enclos_env__"),
      binding   = self,      # AKA the public binding environment
      private   = NULL
    )
  )

  if (!is.environment(old[[1]]$enclosing)) {
    stop("clone() must be called from an R6 object.")
  }

  old[[1]]$private <- old[[1]]$enclosing$private
  has_private <- !is.null(old[[1]]$private)

  # Figure out if we're in a portable class object
  portable <- !identical(old[[1]]$binding, old[[1]]$enclosing)

  # Traverse the super binding and enclosing environments, and add them to the
  # list.
  i <- 1
  while (TRUE) {
    if (is.null(old[[i]]$enclosing$super)) {
      break
    }

    old[[i+1]] <- list(
      binding   = old[[i]]$enclosing$super,
      enclosing = old[[i]]$enclosing$super$.__enclos_env__
    )

    i <- i + 1
  }

  # Set up stuff for deep clones
  if (deep) {
    if (has_private && is.function(old[[1]]$private$deep_clone)) {
      # Get private$deep_clone, if available.
      deep_clone <- old[[1]]$private$deep_clone
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

  # ---------------------------------------------------------------------------
  # Create representation of the new object
  # ---------------------------------------------------------------------------

  # The object representation is made up of a list of "slices". Each slice
  # represents one level of inheritance. The first slice is somewhat different
  # from subsequent ones. The later ones are superclass slices. They do not
  # have a separate `private` environment.

  # Create the first slice. This one is different from the others.
  make_first_new_slice <- function(old_slice, portable) {
    new_slice <- list(
      enclosing = NULL,
      binding   = NULL
    )

    has_private <- !is.null(old_slice$private)

    if (portable) {
      enclosing_parent <- parent.env(old_slice$enclosing)
      binding_parent   <- emptyenv()

      if (has_private) {
        private_parent   <- emptyenv()
        new_slice$private <- new.env(private_parent, hash = FALSE)
      }
      new_slice$binding   <- new.env(binding_parent,   hash = FALSE)
      new_slice$enclosing <- new.env(enclosing_parent, hash = FALSE)

    } else {
      if (has_private) {
        private_parent   <- parent.env(old_slice$private)
        new_slice$private <- new.env(private_parent, hash = FALSE)

        binding_parent   <- new_slice$private
        new_slice$binding <- new.env(binding_parent, hash = FALSE)

      } else {
        binding_parent <- parent.env(old_slice$binding)
        new_slice$binding <- new.env(binding_parent, hash = FALSE)
      }

      new_slice$enclosing <- new_slice$binding
    }

    # Set up self, private, and .__enclos_env__
    new_slice$enclosing$self <- new_slice$binding
    if (has_private) {
      new_slice$enclosing$private <- new_slice$private
    }
    new_slice$binding$.__enclos_env__ <- new_slice$enclosing

    new_slice
  }


  # This creates a slice other than the first one.
  make_new_slice <- function(old_slice, self, private) {
    enclosing_parent <- parent.env(old[[i]]$enclosing)
    binding_parent   <- parent.env(old[[i]]$binding)

    enclosing <- new.env(enclosing_parent, hash = FALSE)
    binding   <- new.env(binding_parent,   hash = FALSE)

    enclosing$self <- self
    if (!is.null(private)) {
      enclosing$private <- private
    }

    binding$.__enclos_env__ <- enclosing

    list(
      enclosing = enclosing,
      binding = binding
    )
  }

  new <- list(
    make_first_new_slice(old[[1]], portable)
  )

  # Mirror the super environments from the old object
  if (length(old) > 1) {
    for (i in seq(2, length(old))) {
      new[[i]] <- make_new_slice(
        old[[i]],
        new[[1]]$binding,
        new[[1]]$private
      )
    }

    # A second pass to add in the `super` to each enclosing environment.
    for (i in seq(1, length(old)-1)) {
      new[[i]]$enclosing$super <- new[[i+1]]$binding
    }
  }

  # ---------------------------------------------------------------------------
  # Copy members from old to new
  # ---------------------------------------------------------------------------
  copy_slice <- function(old_slice, new_slice) {
    # Copy the old objects, fix up method environments, and put them into the
    # new binding environment.
    binding_copies <- as.list.environment(old_slice$binding, all.names = TRUE)

    # Don't copy self, private, super, or .__enclos_env__
    binding_copies <- binding_copies[
      setdiff(names(binding_copies), c("self", "private", "super", ".__enclos_env__"))
    ]

    # TODO: Fix this up to iterate over super envs
    binding_copies <- assign_func_envs(binding_copies, new_slice$enclosing)

    # Separate active and non-active bindings
    active_idx <- vapply(
      names(binding_copies),
      bindingIsActive,
      env = old_slice$binding,
      TRUE
    )
    active_copies  <- binding_copies[active_idx]
    binding_copies <- binding_copies[!active_idx]

    if (deep) {
      binding_copies <- mapply(
        deep_clone,
        names(binding_copies),
        binding_copies,
        SIMPLIFY = FALSE
      )
    }

    # Copy in public and active bindings
    list2env2(binding_copies, new_slice$binding)

    if (length(active_copies) > 0) {
      for (name in names(active_copies)) {
        makeActiveBinding(name, active_copies[[name]], new_slice$binding)
      }
    }

    # Copy private members
    if (!is.null(old_slice$private)) {
      private_copies <- as.list.environment(old_slice$private, all.names = TRUE)
      if (deep) {
        private_copies <- mapply(
          deep_clone,
          names(private_copies),
          private_copies,
          SIMPLIFY = FALSE
        )
      }
      private_copies <- assign_func_envs(private_copies, new_slice$enclosing)
      list2env2(private_copies, new_slice$private)
    }
  }

  for (i in seq_along(old)) {
    # This works because the objects in new are reference objects
    copy_slice(old[[i]], new[[i]])
  }


  class(new[[1]]$binding) <- class(old[[1]]$binding)

  # Lock --------------------------------------------------------------
  # Copy locked state of environment
  if (environmentIsLocked(old[[1]]$binding)) {
    lockEnvironment(new[[1]]$binding)
  }
  if (has_private && environmentIsLocked(old[[1]]$private)) {
    lockEnvironment(new[[1]]$private)
  }

  # Always lock methods
  # R 3.2.0 introduced the sorted=FALSE option, which makes ls() much faster,
  # so at some point we'll be able to switch to that.
  for (name in ls(new[[1]]$binding)) {
    if (is.function(.subset2(new[[1]]$binding, name)))
      lockBinding(name, new[[1]]$binding)
  }
  if (has_private) {
    for (name in names(new[[1]]$private)) {
      if (is.function(new[[1]]$private[[name]]))
        lockBinding(name, new[[1]]$private)
    }
  }

  new[[1]]$binding
}
