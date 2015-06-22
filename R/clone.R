#' Clone an R6 object
#'
#' This function clones an R6 object, with a few limitations. It can be called
#' with \code{clone(obj)}, or you can add a public \code{clone} method to your
#' class, like this: \code{clone = function() clone(self)}.
#'
#' Note that for non-portable R6 classes, \code{clone = function() clone(self)}
#' will result in the method attempting to call itself. In this case, you will
#' need to use \code{clone = function() R6::clone(self)}.
#'
#' @section Limitations:
#'
#'   \code{clone()} can't correctly copy objects with active bindings; the value
#'   of the active binding will be copied, not the active binding itself.
#'
#'   If you try to clone an object that inherits from another class,
#'   \code{clone()} presently won't copy the \code{super} object. This will
#'   cause problems only if you access the \code{super} object explicitly; any
#'   inherited fields or methods should work fine.
#'
#'   \code{clone()} only makes shallow copies. If your object has any fields
#'   with reference semantics (e.g., environments, RefClass objects, or R6
#'   objects), then clone will point to those same objects, not copies of those
#'   objects.
#'
#' @param obj An R6 object to clone.
#'
#' @examples
#' AC <- R6Class("AC",
#'   public = list(
#'     x = 1,
#'     gety = function() private$y,
#'     sety = function(y) private$y <- y
#'   ),
#'   private = list(
#'     y = 1
#'   )
#' )
#'
#' a <- AC$new()
#' b <- clone(a)
#' b$x       # 1
#' b$gety()  # 1
#'
#' # Changing values in a doesn't affect b
#' a$x <- 2
#' a$sety(2)
#' b$x       # 1
#' b$gety()  # 1
#'
#' # Changing values in b does affect b
#' b$x <- 3
#' b$sety(3)
#' b$x       # 3
#' b$gety()  # 3
#'
#'
#'
#' # A class with a built-in clone() method
#' Cloner <- R6Class("Cloner",
#'   public = list(
#'     x = 1,
#'     clone = function() clone(self)
#'   )
#' )
#'
#' a <- Cloner$new()
#' b <- a$clone()
#' a$x <- 2
#' b$x  # 1
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
  public_copies <- as.list.environment(old_public_bind_env, all.names = TRUE)
  public_copies <- assign_func_envs(public_copies, new_enclos_env)
  list2env2(public_copies, public_bind_env)

  if (has_private) {
    private_copies <- as.list.environment(old_private_bind_env, all.names = TRUE)
    private_copies <- assign_func_envs(private_copies, new_enclos_env)
    list2env2(private_copies, private_bind_env)
  }

  # Clone super object -------------------------------------------
  clone_super(old_enclos_env, new_enclos_env, public_bind_env)

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


encapsulate({
  clone_super <- function(old_enclos_env, new_enclos_env, public_bind_env) {
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
    if (!is.null(new_enclos_env$private))
      new_super_enclos_env$private <- new_enclos_env$private

    new_super_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

    # Copy over the methods and fix up their environments
    super_copies <- assign_func_envs(super_copies, new_super_enclos_env)
    list2env2(super_copies, new_super_bind_env)


    new_enclos_env$super <- new_super_bind_env

    # Recurse
    clone_super(old_super_enclos_env, new_super_enclos_env, public_bind_env)
  }
})