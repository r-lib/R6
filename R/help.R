# Inspired by '.refMethodDoc' in src/libary/methods/R/refClass in R3.1.1
# which is provided under the GPLv2 by The R Core Team
get_method_doc <- function(method) {
  bb <- body(method)
  doc <- NULL
  if (is(bb, "{") && length(bb) > 1 && is(bb[[2]], "character") ) {
    doc <- bb[[2]]
  }
  doc
}

# If integrated into an R6 class, this prints the documentation string
# of a method when called with its name as argument.
R6_help <- function(method) {
  if (!is.character(method))
    stop('`method` must be the name of a public method (as character)')

  method.name <- paste0(class(self)[1], '$', method)
  method.func <- self[[method]]
  if (is.null(method.func) || !is.function(method.func))
    stop('help can only be printed for public methods')

  doc <- get_method_doc(method.func)
  if (is.null(doc)) doc <- 'No documentation available.'
  cat(method.name, ': ',  doc, '\n', sep='')
}