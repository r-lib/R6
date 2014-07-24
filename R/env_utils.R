# Search a list for all function objects, change the environment for those
# functions to a target environment, and return the modified list.
assign_func_envs <- function(objs, target_env) {
  if (is.null(target_env)) return(objs)

  lapply(objs, function(x) {
    if (is.function(x)) environment(x) <- target_env
    x
  })
}

# Get names of all superclasses
get_superclassnames <- function(inherit) {
  if (is.null(inherit)) return(NULL)
  c(inherit$classname, get_superclassnames(inherit$inherit))
}

# Wrapper around list2env with a NULL check
list2env2 <- function(x, envir = NULL, parent = emptyenv(),
                       hash = (length(x) >  100),
                       size = max(29L, length(x))) {
  if (is.null(envir)) {
    envir <- new.env(hash = hash, parent = parent, size = size)
  }
  if (is.null(x) || length(x) == 0) return(NULL)
  list2env(x, envir)
}
