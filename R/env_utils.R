# Copy all objects from one environment to another
copy_env <- function(src, dest = new.env(parent = emptyenv())) {
  list2env(as.list.environment(src, all.names = TRUE), envir = dest)
}


# Search an environment for all function objects, and change the environment
# for those functions to another target environment.
assign_func_envs <- function(env, target_env) {
  names <- ls(env, all.names = TRUE)

  for (name in names) {
    if (is.function(env[[name]])) {
      environment(env[[name]]) <- target_env
    }
  }
}


Rcpp::cppFunction("List setEnvs(List fs, Environment env) {
   int n = fs.size();
   List out(n);

   for (int i = 0; i < n; ++i) {
     SEXP f = Rf_shallow_duplicate(fs[i]);
     SET_CLOENV(f, env);
     out[i] = f;
   }

   return out;
}")
