#' Reference class generator, with private and public members
#'
#' Classes created by this generator have the following properties:
#' \itemize{
#'   \item Has private and public members.
#'   \item The private environment is the parent of the public environment.
#'   \item Each object created by the generator's \code{$new} function is the
#'     public environment, with a class.
#'   \item Methods can directly access the public and private environments, by
#'     using \code{public$foo} or \code{private$foo}. Assignment to either
#'     environment can be done with \code{<<-}, but it's more precise to
#'     explicitly specify \code{private} or \code{public}.
#'   \item The execution environment of all methods is set to the public
#'     environment, even if it's a private method. Private methods are found
#'     in the private environment, but they have the public environment as
#'     their execution environment.
#'   \item Each instance of the class has its own copy of each method. (I'm not
#'     sure whether this means that the class is )
#' }
#'
#' @param classname Name of the class.
#' @param private A list of private members, which can be functions and
#'   non-functions.
#' @param public A list of public members, which can be functions and
#'   non-functions.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param lock Should the environments of the generated objects be locked?
#' @examples
#' class5 <- new_class("class5",
#'   private = list(
#'     x = 1,
#'     y = 2,
#'     sum_xz = function() x + z
#'   ),
#'   public = list(
#'     z = 3,
#'     initialize = function(x = NULL, y = NULL, z = NULL) {
#'       if (!is.null(x)) private$x <- x
#'       if (!is.null(y)) private$y <- y
#'       if (!is.null(z)) public$z  <- z
#'     },
#'     # Set a private variable
#'     set_x = function(val) private$x <- val,
#'     # Access private and public variables
#'     sum_xyz = function() x + y + z,
#'     # Access a private variable and private method
#'     sum_xyz2 = function() y + sum_xz()
#'   )
#' )
#'
#' z <- class5$new(11, z = 13)
#'
#' z$sum_xyz()
#' z$sum_xyz2()
#' z$x <- 20    # Error - can't access private member directly
#' z$set_x(20)
#' z$sum_xyz()
#' z$sum_xyz2()
#' z$z <- 100   # Can set public members directly
#' z$sum_xyz()
#'
#' # Can also create S3 methods for prettier printing of class objects
#' # This prints the contents of the public environment
#' print.class5 <- function(x, ...) {
#'   str(as.list.environment(x))
#' }
#' print(z)
#'
new_class <- function(classname, private = list(), public = list(),
                      parent_env = parent.frame(), lock = TRUE) {
  template <- list()
  template$private <- new.env(parent = parent_env)
  template$public  <- new.env(parent = template$private)

  # Copy the private and public list items into the environments
  list2env(private, envir = template$private)
  list2env(public,  envir = template$public)

  # All functions execute in the public env (even if they're found in the
  # private env)
  assign_func_envs(template$private, template$public)
  assign_func_envs(template$public,  template$public)

  lockEnvironment(template$private, bindings = TRUE)
  lockEnvironment(template$public, bindings = TRUE)

  template$new <- generate_new(classname, template, parent_env, lock)

  structure(template, class = paste0(classname, "_generator"))
}

# Returns a $new() function for a class
generate_new <- function(classname = NULL, template = NULL,
                         parent_env = NULL, lock = TRUE) {
  if (is.null(classname) || is.null(template) || is.null(parent_env)) {
    stop("classname, template, and parent_env must be supplied.")
  }

  function(...) {
    # Create private and public environment
    private_env <- new.env(parent = parent_env)
    public_env  <- new.env(parent = private_env)

    # Copy private and public environment from the template
    copy_env(template$private, private_env)
    copy_env(template$public,  public_env)

    # Fix environments for functions
    assign_func_envs(private_env, public_env)
    assign_func_envs(public_env,  public_env)

    # Add .private and .public pointers
    public_env$private  <- private_env
    private_env$private <- private_env
    public_env$public   <- public_env
    private_env$public  <- public_env

    if (lock) {
      lockEnvironment(private_env)
      lockEnvironment(public_env)
    }

    # Run the initalize() method if available
    if (is.function(public_env$initialize)) {
      public_env$initialize(...)
    }

    structure(public_env, class = classname)
  }
}
