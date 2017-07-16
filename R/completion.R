#' Given an R6 generator object, provide completions for R6 objects
#'
#' This function is meant to be used by interactive R programming environments.
#'
#' @param generator An R6 generator object.
#' @param group Which group of completions to supply (public or private members).
#' @param operator The operator for which to provide completions. R6 only uses
#'   \code{$} and \code{[[}.
#'
#' @keywords internal
#' @rdname completion
.instanceCompletions <- function(generator, group = c("public", "private"),
                                 operator = "$") {
  if (! operator %in% c("$", "[[")) {
    return(list())
  }
  group <- match.arg(group)

  if (group == "public") {
    items <- c(
      lapply(names(generator$public_fields), function(name) {
        list(name = name, type = NULL, args = NULL)
      }),
      lapply(names(generator$active), function(name) {
        list(name = name, type = "active", args = NULL)
      }),
      lapply(names(generator$public_methods), function(name) {
        args <- names(formals(generator$public_methods[[name]]))
        args <- setdiff(args, "...")
        list(name = name, type = "function", args = args)
      })
    )
  } else if (group == "private") {
    items <- c(
      lapply(names(generator$private_fields), function(name) {
        list(name = name, type = NULL, args = NULL)
      }),
      lapply(names(generator$private_methods), function(name) {
        args <- names(formals(generator$private_methods[[name]]))
        args <- setdiff(args, "...")
        list(name = name, type = "function", args = args)
      })
    )
  }

  items
}
