# Copy all objects from one environment to another
copy_env <- function(src, dest = new.env(parent = emptyenv())) {
  list2env(as.list.environment(src, all.names = TRUE), envir = dest)
}

# Search a list for all function objects, change the environment for those
# functions to a target environment, and return the modified list.
assign_func_envs <- function(objs, target_env) {
  lapply(objs, function(x) {
    if (is.function(x)) environment(x) <- target_env
    x
  })
}
