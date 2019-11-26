# This function will be added as a method to R6 objects, with the name 'get_sate',
# and with the environment changed, unless an user defined get_state function as
# been added
get_state <- function() {
  # ---------------------------------------------------------------------------
  # Create representation of the current object
  # ---------------------------------------------------------------------------

  instance <- list(
    enclosing = .subset2(self, ".__enclos_env__"),
    binding   = self, # AKA the public binding environment
    private   = NULL
  )

  instance$private <- instance$enclosing$private

  public_binding <- names(instance$binding)

  # Determine which public bindings are active bindings. These will not be copied
  # to the state output
  active_idx <- vapply(public_binding,
                       bindingIsActive,
                       env = instance$binding,
                       TRUE)

  # Don't copy self, private, super, or .__enclos_env__.
  keep_idx <-
    !(public_binding %in% c("self", "private", "super", ".__enclos_env__"))

  public_binding <- public_binding[keep_idx & !active_idx]

  public_copies <- mget(public_binding, envir = instance$binding)

  # Only keep fields as state
  keep_public_fields <-
    !vapply(public_copies, is.function, logical(1))

  private_copies <- list()
  keep_private_fields = TRUE

  if (!is.null(instance$private)) {
    private_copies <-
      as.list.environment(instance$private, all.names = TRUE)

    # Only keep fields as state
    keep_private_fields <-
      !vapply(private_copies, is.function, logical(1))
  }

  return(
    list(
      public = public_copies[keep_public_fields],
      private = private_copies[keep_private_fields]
    )
  )
}


# This function will be added as a method to R6 objects, with the name 'set_state',
# and with the environment changed, unless an user defined set_state function as
# been added
set_state <-
  function(state, ...) {
    instance <- list(
      enclosing = .subset2(self, ".__enclos_env__"),
      binding = self,
      private   = NULL
    )

    instance$private <- instance$enclosing$private
    has_private <- !is.null(instance$private)

    if (any(!names(state[["public"]]) %in% names(instance$binding))) {
      stop("public state is incompatible with R6 class definition.")
    }

    # Overwrite the public fields in the public bindings with the values of state
    list2env(state[["public"]], instance$binding)

    if (has_private && "private" %in% names(state)) {
      if (any(!names(state[["private"]]) %in% names(instance$private))) {
        stop("private state is incompatible with R6 class definition.")
      }

      # Overwrite the private fields in the private bindings with the values of state
      list2env(state[["private"]], instance$private)
    }

    return(self)
  }