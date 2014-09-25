# TODO:
# * X Test portable and non-portable
# * Figure out how to pass in old_enclos_env. Ideally we would call clone(self).
#     May need to store enclos in an attribute?
# * Don't try to clone non-R6 objects
# * Superclass
# * Need better encapsulation strategy
# * Copy locked status - copy all attributes?
# * Be more careful about reassigning function envs - only if matches enclos_env?
#     But this is problematic for inherited functions
# * Be careful about copying reference objects

#' @export
clone <- encapsulate(function(old_enclos_env = parent.env(parent.frame())) {

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
  copies <- as.list.environment(old_public_bind_env)
  copies <- assign_func_envs(copies, new_enclos_env)
  list2env2(copies, public_bind_env)

  if (has_private) {
    copies <- as.list.environment(old_private_bind_env)
    copies <- assign_func_envs(copies, new_enclos_env)
    list2env2(copies, private_bind_env)
  }

  # Add self and (optional) private pointer
  new_enclos_env$self <- public_bind_env
  if (has_private)
    new_enclos_env$private <- private_bind_env

  class(public_bind_env) <- class(old_public_bind_env)
  public_bind_env
})

