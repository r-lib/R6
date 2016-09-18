#' Is an object an R6 Class Generator or Object?
#'
#' Checks for R6 class generators and R6 objects.
#' @param x Any variable.
#' @return A logical value.
#' \code{is.r6} returns \code{TRUE} when the input is an R6 class generator or
#' an R6 object, and \code{FALSE} otherwise.
#' \code{is.r6gen} returns \code{TRUE} when the input is an R6 class generator
#' and \code{FALSE} otherwise.
#' \code{is.r6obj} returns \code{TRUE} when the input is an R6 object and
#' \code{FALSE} otherwise.
#' @examples
#' class_generator <- R6::R6Class()
#' object <- class_generator$new()
#' is.r6(class_generator)
#' is.r6gen(class_generator)
#' is.r6obj(class_generator)
#' is.r6(object)
#' is.r6gen(object)
#' is.r6obj(object)
#' @export
is.r6 <- function(x)
{
  is.r6obj(x) || is.r6gen(x)
}

#' @rdname is.r6
#' @export
is.r6obj <- function(x)
{
  inherits(x, "R6")
}

#' @rdname is.r6
#' @export
is.r6gen <- function(x)
{
  inherits(x, "R6ClassGenerator")
}
