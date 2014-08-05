#' @export
print.R6 <- function(x, ...) {
  if (is.function(x$print)) {
    x$print()

  } else {
    cat(
      "<", class(x)[1], ">\n",
      "  Public:\n",
      indent(object_summaries(x), 4),
      "\n",
      sep = ""
    )

    if (!is.null(x$private)) {
      cat(
        "  Private:\n",
        indent(object_summaries(x$private), 4),
        "\n",
        sep = ""
      )
    }
    invisible(x)
  }
}

#' @export
print.R6ClassGenerator <- function(x, ...) {
  classname <- x$classname
  if (is.null(classname)) classname <- "unnamed"
  cat(
    "<", classname, "> object generator\n",
    "  Public:\n",
    indent(object_summaries(x$public_fields), 4),
    indent(object_summaries(x$public_methods), 4),
    sep = ""
  )

  if (!is.null(x$active)) {
    cat(
      "  Active bindings:\n",
      indent(object_summaries(x$active), 4),
      sep = ""
    )
  }

  if (!(is.null(x$private_fields) && is.null(x$private_methods))) {
    cat(
      "  Private:\n",
      indent(object_summaries(x$private_fields), 4),
      indent(object_summaries(x$private_methods), 4),
      sep = ""
    )
  }
  cat("  Parent env: ", format(x$parent_env), "\n", sep = "")
  cat("  Lock: ", x$lock, "\n", sep = "")
  cat("  Portable: ", x$portable, "\n", sep = "")
  invisible(x)
}

# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- names(x)
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  values <- vapply(obj_names, function(name) {
    obj <- x[[name]]
    if (is.environment(x) && bindingIsActive(name, x)) "active binding"
    else if (is.function(obj)) "function"
    else if (is.environment(obj)) "environment"
    else if (is.atomic(obj)) trim(paste(as.character(obj), collapse = " "))
    else paste(class(obj), collapse = ", ")
  }, FUN.VALUE = character(1))

  paste0(
    paste0(obj_names, ": ", values, sep = "", collapse = "\n"),
    "\n"
  )
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
