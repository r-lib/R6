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
#' Class4 <- createRefClass("Class4",
#'   members = list(
#'     x = 1,
#'     y = 2,
#'     initialize = function(x = NULL, y = NULL) {
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
#' z <- Class4$new(y = 10)
#'
#' z$sum_xy()
#' z$x <- 20    # Set member directly
#' z$sum_xy()
#' z$set_x(40)  # Set member with setter function
#' z$set_y(60)
#' z$sum_xy()
#' z$sum_xy2()
#'
#' # Print, using the print.RefClass method:
#' print(z)
createRefClass <- function(classname, members = list(),
                           parent_env = parent.frame(), lock = TRUE) {

  newfun <- function(...) {
    env <- list2env(members, parent = parent_env)

    # Fix environment for functions
    assign_func_envs(env, env)

    # Add self pointer
    env$self <- env

    if (lock) lockEnvironment(env)
    if (is.function(env$initialize)) env$initialize(...)

    structure(env, class = c(classname, "RefClass"))
  }

  structure(
    list(new = newfun),
    class = "RefClassGenerator",
    classname = classname
  )
}

#' A rough way of printing out the contents of a RefClass object
#' @export
print.RefClass <- function(x, ...) {
  names <- ls(x, all.names = TRUE)
  values <- vapply(names, function(name) {
    obj <- x[[name]]
    if (is.function(obj)) "function"
    else if (is.environment(obj)) "environment"
    else as.character(obj)
  }, FUN.VALUE = character(1))

  cat(
    class(x)[1], " object\n",
    paste("  ", names, ": ", values, sep = "", collapse = "\n"),
    sep = ""
  )
}

#' @export
print.RefClassGenerator <- function(x, ...) {
  cat(attr(x, "classname"), "object generator")
}
