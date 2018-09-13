R6 classes
===========

[![Build Status](https://travis-ci.org/r-lib/R6.svg?branch=master)](https://travis-ci.org/r-lib/R6) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-lib/R6?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/R6)

This package contains an implemention of classes with reference semantics, and it is a simpler, faster, lighter-weight alternative to R's built-in reference classes.

Additionally, these classes are not built on S4 classes, so they do not require the methods package. They allow public and private members, and they support inheritance. Unlike reference classes, R6 classes can be cleanly inherited across different packages, when used in portable mode (enabled by default).

Why the name R6? When R's reference classes were introduced, some users, following the names of R's existing class systems S3 and S4, called the new class system R5 in jest. Although reference classes are not actually called R5, the name of this package and its classes takes inspiration from that name.

The name R5 was also a code-name used for a different object system started by Simon Urbanek, meant to solve some issues with S4 relating to syntax and performance. However, the R5 branch was shelved after a little development, and it was never released.

## Installation

To install R6 from CRAN:

```R
install.packages('R6')
```

To install the development version (requires the devtools package):

```R
devtools::install_github('r-lib/R6', build_vignettes = FALSE)
```


## Documentation

* [Introduction to R6](articles/Introduction.html)
* [Debugging methods in R6 objects](articles/Debugging.html)
* [Performance tests](articles/Performance.html) - Speed and memory comparisons of R6 classes and reference classes.
* [Portable R6 classes](articles/Portable.html) - Inheritance across different packages.
