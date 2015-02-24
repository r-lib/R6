#' @export
`$.R6` <- function(x, y){
  if(!exists(y, x, inherits = FALSE)){
    stop("R6 class '", is(x), "' has no public field '", y, "'.", call. = FALSE)
  }
  get(y, x, inherits = FALSE)
}

