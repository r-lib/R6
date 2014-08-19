library(testthat)
context("Inheritance across packages")

## Helper functions to create a new package, with some
## R code, and install it temporarily

install_quietly <- TRUE

with_wd <- function(dir, expr) {
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(dir)
  eval(substitute(expr), envir = parent.frame())
}

build_pkg <- function(path, pkg_file = NULL) {
  if (!file.exists(path)) stop("path does not exist")
  pkg_name <- basename(path)
  if (is.null(pkg_file)) {
    pkg_file <- file.path(dirname(path), paste0(pkg_name, "_1.0.tar.gz"))
  }
  with_wd(dirname(path),
          tar(basename(pkg_file), pkg_name, compression = "gzip"))
  pkg_file
}

install_tmp_pkg <- function(..., pkg_name, lib_dir, imports = "R6") {
  if (!file.exists(lib_dir)) stop("lib_dir does not exist")
  if (!is.character(pkg_name) || length(pkg_name) != 1) {
    stop("pkg_name is not a string")
  }

  ## Create a directory that will contain the source package
  src_dir <- tempfile()
  on.exit(try(unlink(src_dir, recursive = TRUE), silent = TRUE), add = TRUE)
  dir.create(src_dir)

  ## Create source package, need a non-empty environment,
  ## otherwise package.skeleton fails
  tmp_env <- new.env()
  assign("f", function(x) x, envir = tmp_env)
  suppressMessages(package.skeleton(pkg_name, path = src_dir,
                                    envir = tmp_env))
  pkg_dir <- file.path(src_dir, pkg_name)

  ## Make it installable: remove man, add R6 dependency
  unlink(file.path(pkg_dir, "man"), recursive = TRUE)
  cat("Imports: ", paste(imports, collapse = ", "), "\n",
      file = file.path(pkg_dir, "DESCRIPTION"), append = TRUE)
  cat(paste0("import(", imports, ")"), sep="\n",
      file = file.path(pkg_dir, "NAMESPACE"), append = TRUE)

  ## Put the code in it, dput is noisy, so we need to redirect it to
  ## temporary file
  exprs <- list(...)
  unlink(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "R"))
  code_file <- file.path(pkg_dir, "R", "code.R")
  tmp_file <- tempfile()
  on.exit(try(unlink(tmp_file), silent = TRUE), add = TRUE)
  sapply(exprs, function(x)
         cat(deparse(dput(x, file = tmp_file)),
             file = code_file, append = TRUE, "\n", sep="\n"))

  ## Build it
  pkg_file <- build_pkg(pkg_dir)

  ## Install it into the supplied lib_dir
  install.packages(pkg_file, lib = lib_dir, repos = NULL, type = "source",
                   quiet = install_quietly)
}

with_libpath <- function(lib_path, ...) {
  cur_lib_path <- .libPaths()
  on.exit(.libPaths(cur_lib_path), add = TRUE)
  .libPaths(c(lib_path, cur_lib_path))
  exprs <- c(as.list(match.call(expand.dots = FALSE)$...))
   sapply(exprs, eval, envir = parent.frame())
}

## Each expression in ... is put in a package, that
## is installed and loaded. The package name is given by
## argument name. The packages will be installed in lib_dir,
load_tmp_pkgs <- function(..., lib_dir = tempfile(), imports = "R6") {
  if (!file.exists(lib_dir)) dir.create(lib_dir)
  exprs <- c(as.list(match.call(expand.dots = FALSE)$...))
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    name <- names(exprs)[i]
    install_tmp_pkg(expr, pkg_name = name,
                    lib_dir = lib_dir, imports = imports)
    ## Unload everything if an error happens
    on.exit(try(unloadNamespace(name), silent = TRUE), add = TRUE)
    with_libpath(lib_dir, suppressMessages(library(name, quietly = TRUE,
                                                   character.only = TRUE)))
    on.exit()
  }
  invisible(NULL)
}

test_that("inheritance works across packages", {

  ## Temporary lib_dir
  lib_dir <- tempfile()
  on.exit(try(unlink(lib_dir, recursive = TRUE), silent = TRUE),
          add = TRUE)
  on.exit(unloadNamespace("R6testB"), add = TRUE)
  on.exit(unloadNamespace("R6testA"), add = TRUE)

  ## Make sure that we get the latest versions of them
  try(unloadNamespace("R6testB"), silent = TRUE)
  try(unloadNamespace("R6testA"), silent = TRUE)

  load_tmp_pkgs(lib_dir = lib_dir,

    ## Code to put in package 'R6testA'
    R6testA = {
      AC <- R6Class(
        public = list(
          x = 1
        )
      )
    },

    ## Code to put in package 'R6testB'
    R6testB = {
      BC <- R6Class(
        inherit = R6testA::AC,
        public = list(
          y = 2
        )
      )
    }

  )

  ## Now ready for the tests
  B <- BC$new()
  expect_equal(B$x, 1)
  expect_equal(B$y, 2)

})

test_that("more inheritance", {

  ## Temporary lib_dir
  lib_dir <- tempfile()

  on.exit(try(unlink(lib_dir, recursive = TRUE), silent = TRUE), add = TRUE)
  on.exit(unloadNamespace("pkgB"), add = TRUE)
  on.exit(unloadNamespace("pkgA"), add = TRUE)

  ## Make sure that we get the latest versions of them
  try(unloadNamespace("pkgB"), silent = TRUE)
  try(unloadNamespace("pkgA"), silent = TRUE)

  load_tmp_pkgs(lib_dir = lib_dir,
    pkgA = {
      funA <- function() {
        message("Called funA in pkgA 1.0")
      }
      AC <- R6Class("AC",
        public = list(
          versionString = "pkgA 1.0",
          fun = function() {
            message("This object was created in pkgA 1.0")
            message(paste0("The object has versionString ",
                           self$versionString))
            funA()
          }
        )
      )
    }
  )

  load_tmp_pkgs(lib_dir = lib_dir, imports = "pkgA",
    pkgB = {
      B <- pkgA::AC$new()
    }
  )

  expect_message(B$fun(), "created in pkgA 1.0")
  expect_message(B$fun(), "versionString pkgA 1.0")
  expect_message(B$fun(), "Called funA in pkgA 1.0")

  unloadNamespace("pkgB")
  unloadNamespace("pkgA")

  load_tmp_pkgs(lib_dir = lib_dir,
    pkgA = {
      funA <- function() {
        message("Called funA in pkgA 2.0")
      }
      AC <- R6Class("AC",
        public = list(
          versionString = "pkgA 2.0",
          fun = function() {
            message("This object was created in pkgA 2.0")
            message(paste0("The object has versionString ",
                           self$versionString))
            funA()
          }
        )
      )
    }
  )

  with_libpath(lib_dir, library(pkgB))

  expect_message(B$fun(), "created in pkgA 1.0")
  expect_message(B$fun(), "versionString pkgA 1.0")
  expect_message(B$fun(), "Called funA in pkgA 2.0")

})
