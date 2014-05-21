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
#'   \item Active bindings can be used to call a function that looks like an
#'     object.
#'   \item The execution environment of all methods is set to the public
#'     environment, even if it's a private method. Private methods are found
#'     in the private environment, but they have the public environment as
#'     their execution environment.
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
#' @param private A list of private members, which can be functions and
#'   non-functions.
#' @param public A list of public members, which can be functions and
#'   non-functions.
#' @param active An optional list of active binding functions.
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
#'   ),
#'   active = list(
#'     x2 = function(value) {
#'       if (missing(value)) return(x * 2)
#'       else private$x <- value/2
#'     }
#'   )
#' )
#'
#' z <- Class5$new(11, z = 13)
#'
#' z$sum_xyz()
#' z$sum_xyz2()
#' # z$x <- 20  # Error - can't access private member directly
#' z$set_x(20)
#' z$sum_xyz()
#' z$sum_xyz2()
#' z$z <- 100   # Can set public members directly
#' z$sum_xyz()
#'
#' z$x2         # An active binding that returns x*2
#' z$x2 <- 1000 # Setting an active binding
#' z$sum_xyz()  # 515
#'
#' # Print, using the print.RefClass2 method:
#' print(z)
createRefClass2 <- function(classname = NULL, private = list(),
                            public = list(), active = NULL,
                            parent_env = parent.frame(), lock = TRUE) {

  newfun <- function(...) {
    private_env <- new.env(parent = parent_env, hash = (length(private) > 100))
    public_env <- new.env(parent = private_env, hash = (length(public) > 100))

    # Fix environment for functions
    private <- assign_func_envs(private, public_env)
    public <- assign_func_envs(public, public_env)

    # Copy objects to environments
    list2env(private, envir = private_env)
    list2env(public, envir = public_env)

    # Add self pointers
    private_env$private <- private_env
    private_env$public  <- public_env
    public_env$private  <- private_env
    public_env$public   <- public_env

    if (!is.null(active)) {
      active <- assign_func_envs(active, public_env)

      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], public_env)
      }
    }

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
