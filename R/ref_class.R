#' Reference class generator, with all public members
#'
#' Classes created by this generator have the following properties:
#' \itemize{
#'   \item Has public members only (no private).
#'   \item Each object created by the generator's \code{$new} function is the
#'     public environment, with a class.
#'   \item Methods can directly access the public (or object) environment, by
#'     using \code{self$foo}. Assignment can be done with \code{<<-}, but it's
#'     more precise to explicitly use \code{self}.
#'   \item The execution environment of all methods is set to the public
#'     environment.
#'   \item Each instance of the class has its own copy of each method. I'm not
#'     sure how large the memory footprint is for this; each copy of a method
#'     is exactly the same except for the environment.
#' }
#'
#' @param classname Name of the class.
#' @param members A list of public members, which can be functions and
#'   non-functions.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param lock Should the environments of the generated objects be locked?
#' @examples
#' class4 <- create_ref_class("class4",
#'   members = list(
#'     x = 1,
#'     y = 2,
#'     initialize = function(x = NULL, y = NULL, z = NULL) {
#'       if (!is.null(x)) self$x <- x
#'       if (!is.null(y)) self$y <- y
#'     },
#'     # Set a variable
#'     set_x = function(value) self$x <- value,
#'     # Set a variable with <<-
#'     set_y = function(value) y <<- value,
#'     # Access variables
#'     sum_xy = function() x + y,
#'     # Access variables and a method
#'     sum_xy2 = function() x + y + sum_xy()
#'   )
#' )
#'
#' # Create a new object with a specified value for y
#' z <- class4$new(y = 10)
#'
#' z$sum_xy()
#' z$x <- 20    # Set member directly
#' z$sum_xy()
#' z$set_x(40)  # Set member with setter function
#' z$set_y(60)
#' z$sum_xy()
#' z$sum_xy2()
#'
#' # Can also create S3 methods for prettier printing of class objects.
#' # This prints the contents of the public environment.
#' print.class4 <- function(x, ...) {
#'   str(as.list.environment(x))
#' }
#' print(z)
#'
create_ref_class <- function(classname, members = list(),
                          parent_env = parent.frame(), lock = TRUE) {
  template <- list()
  template$public <- new.env(parent = parent_env)

  # Copy the public list items into the environment
  list2env(members, envir = template$public)

  # All functions execute in the public env (even if they're found in the
  # private env)
  assign_func_envs(template$public, template$public)

  lockEnvironment(template$public, bindings = TRUE)

  template$new <- generate_ref_class_new(classname, template, parent_env, lock)

  structure(template, class = paste0(classname, "_generator"))
}

# Returns a $new() function for a class
generate_ref_class_new <- function(classname = NULL, template = NULL,
                                  parent_env = NULL, lock = TRUE) {
  if (is.null(classname) || is.null(template) || is.null(parent_env)) {
    stop("classname, template, and parent_env must be supplied.")
  }

  function(...) {
    # Create public environment
    public_env <- new.env(parent = parent_env)

    # Copy public environment from the template
    copy_env(template$public, public_env)

    # Fix environment for functions
    assign_func_envs(public_env, public_env)

    # Add self pointer
    public_env$self <- public_env

    if (lock) {
      lockEnvironment(public_env)
    }

    # Run the initalize() method if available
    if (is.function(public_env$initialize)) {
      public_env$initialize(...)
    }

    structure(public_env, class = classname)
  }
}
