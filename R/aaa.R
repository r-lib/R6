# This is the enclosing environment for all of the functions involved in
# instantiating objects. It is also the binding environment for all these
# functions, except for R6Class(). This is because a generator object can be
# saved (in a built package, for example) and then restored in a different R
# session which has a different version of the R6 package. With the capsule
# environment, the generator object doesn't need to use any functions or objects
# from the potentially different R6 namespace, and because the saved/restored
# object also saves and restores the capsule environment (but not the R6
# namespace).
capsule <- new.env(hash = FALSE)

# This function takes an expression and evaluates it in the capsule environment.
encapsulate <- function(expr) {
  expr <- substitute(expr)
  eval(expr, capsule)
}
