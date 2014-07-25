R6 classes
===========

[![Build Status](https://travis-ci.org/wch/R6.png?branch=master)](https://travis-ci.org/wch/R6)

This package contains an implemention of classes with reference semantics, and it is a simpler, faster, lighter-weight alternative to R's built-in reference classes.

Additionally, these classes are not built on S4 classes, so they do not require the methods package. They allow public and private members, and they support inheritance.

Why the name R6? When R's reference classes were introduced, some users, following the names of R's existing class systems S3 and S4, called the new class system R5 in jest. Although reference classes are not actually called R5, the name of this package and its classes takes inspiration from that name.

To install this package in R:

```R
install.packages('R6')
```


## Documentation

* [Introduction to R6](http://cran.rstudio.com/web/packages/R6/vignettes/Introduction.html)
* [Performance tests](http://rpubs.com/wch/17459) - Speed and memory comparisons of R6 classes and reference classes.
