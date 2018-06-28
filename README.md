R6 dummy package
================

This is a version of the R6 package that is empty; it has no R code. It is intended to be used for testing the following: If a package calls `R6Class()` at build time and saves the resulting `R6ClassGenerator` object, will that package call any code from the R6 package after it is built? The goal is for the answer to be "no."

For more information, see https://github.com/r-lib/R6/issues/149.

To test this, do the following:

* Install the usual version of R6.
* Install the package which uses `R6Class()` at build time.
* Install this dummy version of the package:

    ```R
    devtools::install_github("r-lib/R6@dummy-package")
    ```

* Restart R.
* Load and test your package.
