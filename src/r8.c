#include <R.h>
#include <Rdefines.h>

SEXP subset_R8(SEXP x, SEXP name) {
  // Look in x (an environment) for the object
  SEXP nameSym = Rf_install(CHAR(STRING_ELT(name, 0)));
  SEXP foundVar = Rf_findVarInFrame(x, nameSym);
  if (foundVar != R_UnboundValue) {
    return foundVar;
  }

  // if not found in x, look in methods
  SEXP methods = Rf_getAttrib(x, Rf_install("methods"));
  if (methods == R_NilValue) {
    return R_NilValue;
  }
  SEXP fun = Rf_findVarInFrame(methods, nameSym);
  if (fun == R_UnboundValue) {
    return R_NilValue;
  }

  // Make a copy of the function, with a new environment
  SEXP fun2 = PROTECT(duplicate(fun));
  SEXP eval_env = Rf_getAttrib(x, Rf_install("eval_env"));
  if (eval_env == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SET_CLOENV(fun2, eval_env);
  UNPROTECT(1);
  return fun2;
}
