
# broadcast 0.0.0.9000
* Initial GitHub Publication


# broadcast 0.0.0.9001
* Provided 2 ways to overload the base maths (+, -, *, et.c) and logical (&, |) operators.
One is the `bc()` function to evaluate a maths expression.
The other is the `broadcaster` class which comes with method dispatch for said operators.


# broadcast 0.0.0.9002
* Most broadcast functions and their overloaded counterparts now support zero-length input, and added tests for this.
* Added the `%/%` operator to `bc.i()`, added overload support for `%/%`, and added tests for these also.
* Added `bc.raw()`, added overload support for bit-wise operators (`&` `|`), and added tests for these also.
* `bc_ifelse()` now takes the dimnames from the `test` argument if `dim(test)` is equal to `bc_dim(yes, no)`.
* Renamed `bc()` to `bc_chain()`.


# broadcast 0.0.0.9003
* Added overload support for the relational (==, !=, etc.) operators, and added tests for these also.


# broadcast 0.0.0.9004
* Overhauled the operators for `bc.raw()`.
* Added `bc.bit()`, for bit-wise operations.
* Small speed improvement for `acast()`.
* Re-written some internal code in 'C'.
* `bc_ifelse()` and `bc.b()` can now also handle type of `raw` in its arguments.


# broadcast 0.0.0.9005
* Removed `bc_chain()`, and adjusted the documentation and tests accordingly.
* Added more tests.


# broadcast 0.0.0.9006
* Almost all functions are now S3 or S4 generics; methods can now be written for them.
* Removed the `bc.num` alias (documenting aliases of S4 generics seems problematic).


# broadcast 0.0.0.9007
* Re-ran the benchmarks involving the `bc.*` functions, as these functions have changed since the last benchmarking.


# broadcast 0.0.0.9008
* Some minor tweaks in the compiled code.
* Improved the speed of the `bc.str()` function.
* Improved some of the examples.
* Fixed some small mistakes in the documentation.
* Fixed incorrect links on the website.
* Registered the 'broadcaster' class to the S4 system.


# broadcast 0.0.0.9009
* Added an additional help page to the Reference Manual explaining the operator overloading.
* Added more examples
* Re-written some 'C++' code to 'C'.
* Added simple linear algebra functions for statistics.
* Added more tests
* Default value for the `ndim2bc` argument in `bind_array()` set to `16L`.


# broadcast 0.0.0.9010
* Improved the documentation a bit.
* Generalized `sd_gauss_lc()` to `sd_lc()`


# broadcast 0.0.0.9011
* `sd_lc()` now also accepts integer and logical types for argument `X`.
* Improved the speed of `sd_lc()` by using a slightly different mathematical formula.


# broadcast 0.0.0.9012
* Added new casting functions: `hier2dim()`, `cast_hier2dim()`, `cast_dim2hier()`, `cast_dim2flat()`, `dropnests()`.

