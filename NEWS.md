# R6 1.0.1.9002

* [BREAKING CHANGE] Added `portable` option, which allows inheritance across
  different package namespaces, and made it the default.

* Inheritance of superclasses is dynamic; instead of reading in the superclass
  when a class is created, this happens each time an object is instantiated.
  (Fixes #12)

* Added trailing newline when printing R6 objects. (Thanks to Gabor Csardi)

* The `print` method of R6 objects can be redefined. (Thanks to Gabor Csardi)

# R6 1.0.1

* First release on CRAN.

* Removed pryr from suggested packages.

# R6 1.0

* First release
