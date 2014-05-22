#' @export
print.RefClass <- function(x, ...) {
  cat("<", class(x)[1], ">\n", sep = "")

  if (is.environment(x$private)) {
    cat(
      "  Private:\n",
      indent(object_summaries(x$private), 4),
      "\n",
      sep = ""
    )
  }

  cat(
    "  Public:\n",
    indent(object_summaries(x), 4),
    sep = ""
  )
}

#' @export
print.RefClassGenerator <- function(x, ...) {
  classname <- x$classname
  if (is.null(classname)) classname <- "unnamed"
  cat("<", classname, "> object generator", sep = "")
}

# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x) {
  if (is.list(x))
    names <- names(x)
  else if (is.environment(x))
    names <- ls(x, all.names = TRUE)

  values <- vapply(names, function(name) {
    obj <- x[[name]]
    if (is.environment(x) && bindingIsActive(name, x)) "active binding"
    else if (is.function(obj)) "function"
    else if (is.environment(obj)) "environment"
    else if (is.atomic(obj)) trim(paste(as.character(obj), collapse = " "))
    else class(obj)
  }, FUN.VALUE = character(1))

  paste0(names, ": ", values, sep = "", collapse = "\n")
}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
    paste0("\\1", paste(rep(" ", indent), collapse = "")),
    str,
    perl = TRUE
  )
}

# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim <- function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, 56), "...")
  else str
}
