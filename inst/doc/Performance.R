
## ------------------------------------------------------------------------
library(microbenchmark)
library(pryr)
library(testclasses)


## ------------------------------------------------------------------------
A_rc <- setRefClass("A_rc", 
  fields = list(x = "numeric"),
  methods = list(
    initialize = function(x = 1) .self$x <<- x,
    inc = function(n = 1) x <<- x + n
  )
)


## ------------------------------------------------------------------------
B_wrc <- createRefClass("B_wrc",
  members = list(
    x = NULL,
    initialize = function(x = 1) self$x <<- x,
    inc = function(n = 1) x <<- x + n
  )
)


## ------------------------------------------------------------------------
print(B_wrc$new())


## ------------------------------------------------------------------------
C_closure_class <- function(x = 1) {
  inc <- function(n = 1) x <<- x + n
  structure(environment(), class = "D_closure")
}


## ------------------------------------------------------------------------
D_closure_noclass <- function(x = 1) {
  inc <- function(n = 1) x <<- x + n
  environment()
}


## ------------------------------------------------------------------------
# Utility functions for calculating sizes
obj_size <- function(newfun) {
  data.frame(
    one = as.numeric(object_size(newfun())),
    incremental = as.numeric(object_size(newfun(), newfun()) - object_size(newfun()))
  )
}

obj_sizes <- function(...) {
  dots <- list(...)
  sizes <- Map(obj_size, dots)
  do.call(rbind, sizes)
}


## ------------------------------------------------------------------------
obj_sizes(
  A_rc = A_rc$new,
  B_wrc = B_wrc$new,
  C_closure_class = C_closure_class,
  D_closure_noclass = D_closure_noclass
)


## ------------------------------------------------------------------------
# Garbage collect now so that we (probably) won't do it in the middle of a run
invisible(gc())
speed <- microbenchmark(
  A_rc = A_rc$new(),
  B_wrc = B_wrc$new(),
  C_closure_class = C_closure_class(),
  D_closure_noclass = D_closure_noclass(),
  unit = "ns"
)
speed


## ------------------------------------------------------------------------
A <- A_rc$new()
B <- B_wrc$new()
C <- C_closure_class()
D <- D_closure_noclass()

invisible(gc())
microbenchmark(
  A_rc = A$inc(),
  B_wrc = B$inc(),
  C_closure_class = C$inc(),
  D_closure_noclass = D$inc(),
  unit = "ns"
)


## ------------------------------------------------------------------------
# Really create a reference object without a class
B2 <- B_wrc$new()
class(B2) <- NULL
microbenchmark(
  B_wrc_noclass = B2$inc(),
  D_closure_noclass = D$inc(),
  unit = "ns"
)


## ------------------------------------------------------------------------
rc_no_self <- setRefClass("rc_no_self", 
  fields = list(x = "numeric"),
  methods = list(
    initialize = function(x = 1) .self$x <<- x,
    inc = function(n = 1) x <<- x + n
  )
)

rc_self <- setRefClass("rc_self", 
  fields = list(x = "numeric"),
  methods = list(
    initialize = function(x = 1) .self$x <<- x,
    inc = function(n = 1) .self$x <- x + n
  )
)


## ------------------------------------------------------------------------
wrc_no_self <- createRefClass("wrc_no_self",
  members = list(
    x = 1,
    inc = function(n = 1) x <<- x + n
  )
)

wrc_self <- createRefClass("wrc_self",
  members = list(
    x = 1,
    inc = function(n = 1) self$x <- self$x + n
  )
)


## ------------------------------------------------------------------------
rc_no_self_obj <- rc_no_self$new()
rc_self_obj <- rc_self$new()
wrc_no_self_obj <- wrc_no_self$new()
wrc_self_obj <- wrc_self$new()

invisible(gc())
microbenchmark(
  rc_no_self = rc_no_self_obj$inc(),
  rc_self = rc_self_obj$inc(),
  wrc_no_self = wrc_no_self_obj$inc(),
  wrc_self = wrc_self_obj$inc(),
  unit = "ns"
)


## ------------------------------------------------------------------------
list_noclass <- list(x = 10)
list_class <- structure(list(x = 10), class = "foo")
env_noclass <- new.env()
env_noclass$x <- 10
env_class <- structure(new.env(), class = "foo")
env_class$x <- 10

invisible(gc())
microbenchmark(
  list_noclass = list_noclass,
  list_class = list_class$x,
  env_noclass = env_noclass,
  env_class = env_class$x
)

