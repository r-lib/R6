#include <R.h>
#include <Rdefines.h>

SEXP get_function_from_env_attrib(SEXP x, SEXP attribSym, SEXP nameSym) {
  SEXP methods_env = Rf_getAttrib(x, attribSym);
  if (isEnvironment(methods_env)) {
    return Rf_findVarInFrame(methods_env, nameSym);
  }
  return R_NilValue;
}

SEXP subset_R8(SEXP x, SEXP name) {
  // Look in x (an environment) for the object
  SEXP nameSym = Rf_install(CHAR(STRING_ELT(name, 0)));
  SEXP foundVar = Rf_findVarInFrame(x, nameSym);
  if (foundVar != R_UnboundValue) {
    return foundVar;
  }

  // if not found in x, look in methods
  SEXP fun = get_function_from_env_attrib(x, Rf_install("methods"), nameSym);

  // If not found in methods, search in methods2. This is present only for
  // storing private methods in a superclass.
  if (!isFunction(fun)) {
    fun = get_function_from_env_attrib(x, Rf_install("methods2"), nameSym);
  }
  if (!isFunction(fun)) {
    return R_NilValue;
  }

  // Make a copy of the function, with a new environment
  SEXP fun2 = PROTECT(duplicate(fun));
  SEXP eval_env = Rf_getAttrib(x, Rf_install("eval_env"));
  if (!isEnvironment(eval_env)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SET_CLOENV(fun2, eval_env);
  UNPROTECT(1);
  return fun2;
}
