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
#'   \item Each instance of the class has its own copy of each method. I'm not
#'     sure how large the memory footprint is for this; each copy of a method
#'     is exactly the same except for the environment.
#' }
#'
#' @export
#' @param classname Name of the class.
#' @param private A list of private members, which can be functions and
#'   non-functions.
#' @param public A list of public members, which can be functions and
#'   non-functions.
#' @param parent_env An environment to use as the parent of newly-created
#'   objects.
#' @param lock Should the environments of the generated objects be locked?
#' @examples
#' Class5 <- createRefClass2("Class5",
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
#'     set_x = function(value) private$x <- value,
#'     # Access private and public variables
#'     sum_xyz = function() x + y + z,
#'     # Access a private variable and private method
#'     sum_xyz2 = function() y + sum_xz()
#'   )
#' )
#'
#' z <- Class5$new(11, z = 13)
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
#' # Print, using the print.RefClass2 method:
#' print(z)
createRefClass2 <- function(classname = NULL, private = list(), public = list(),
                           parent_env = parent.frame(), lock = TRUE) {

  newfun <- function(...) {
    private_env <- list2env(private, parent = parent_env)
    public_env <- list2env(public, parent = private_env)

    # Fix environment for functions
    assign_func_envs(private_env, public_env)
    assign_func_envs(public_env, public_env)

    # Add self pointers
    private_env$private <- private_env
    private_env$public  <- public_env
    public_env$private  <- private_env
    public_env$public   <- public_env

    if (lock) {
      lockEnvironment(private_env)
      lockEnvironment(public_env)
    }
    if (is.function(public_env$initialize)) public_env$initialize(...)

    class(public_env) <- c(classname, "RefClass2")
    public_env
  }

  structure(
    list(new = newfun, classname = classname),
    class = "RefClass2Generator"
  )
}

#' A rough way of printing out the contents of a RefClass object
#' @export
print.RefClass2 <- function(x, ...) {
  cat(
    "<", class(x)[1], ">\n  Private:\n",
    indent(object_summaries(parent.env(x)), 4),
    "\n  Public:\n",
    indent(object_summaries(x), 4),
    sep = ""
  )
}

#' @export
print.RefClass2Generator <- function(x, ...) {
  classname <- x$classname
  if (is.null(classname)) classname <- "unnamed"
  cat("<", classname, "> object generator 2", sep = "")
}
