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
#'   \item Active bindings can be used to call a function that looks like an
#'     object.
#'   \item The execution environment of all methods is set to the public
#'     environment.
#'   \item Each instance of the class has its own copy of each method. The
#'     memory cost of this is small; it should be 56 bytes per method.
#' }
#'
#' The \code{active} argument is a list of active binding functions. These
#' functions take one argument. They look like regular variables, but when
#' accessed, a function is called with an optional argument. For example,
#' if \code{obj$x2} is an active binding, then when accessed as \code{obj$x2},
#' it calls the \code{x2()} function that was in the \code{active} list, with
#' no arguments. However, if a value is assigned to it, as in
#' \code{obj$x2 <- 50}, then the function is called with the right-side value
#' as its argument, as in \code{x2(50)}.
#'
#' @seealso \code{\link{makeActiveBinding}}
#' @export
#' @param classname Name of the class.
#' @param members A list of public members, which can be functions and
#'   non-functions.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param active An optional list of active binding functions.
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
#'   ),
#'   active = list(
#'     x2 = function(value) {
#'       if (missing(value)) return(x * 2)
#'       else self$x <- value/2
#'     }
#'   )
#' )
#'
#' # Create a new object with a specified value for y
#' z <- Class4$new(y = 10)
#'
#' z$sum_xy()
#' # z$x <- 20  # Set member directly
#' z$sum_xy()
#' z$set_x(40)  # Set member with setter function
#' z$set_y(60)
#' z$sum_xy()
#' z$sum_xy2()
#'
#' z$x2         # An active binding that returns x*2
#' z$x2 <- 100  # Setting an active binding
#' z$x          # 50
#'
#' # Print, using the print.RefClass method:
#' print(z)
createRefClass <- function(classname = NULL, members = list(), active = NULL,
                           parent_env = parent.frame(), lock = TRUE) {

  newfun <- function(...) {
    env <- new.env(parent = parent_env)

    # Fix environment for functions
    members <- assign_func_envs(members, env)

    # Copy objects to environments
    list2env(members, envir = env)

    env$self <- env

    if (!is.null(active)) {
      active <- assign_func_envs(active, env)

      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], env)
      }
    }

    if (lock) lockEnvironment(env)
    if (is.function(env$initialize)) env$initialize(...)

    class(env) <- c(classname, "RefClass")
    env
  }

  structure(
    list(new = newfun, classname = classname),
    class = "RefClassGenerator"
  )
}

#' @export
print.RefClass <- function(x, ...) {
  cat(
    "<", class(x)[1], ">\n",
    indent(object_summaries(x), 2),
    sep = ""
  )
}


#' @export
print.RefClassGenerator <- function(x, ...) {
  classname <- x$classname
  if (is.null(classname)) classname <- "unnamed"
  cat("<", classname, "> object generator", sep = "")
}
