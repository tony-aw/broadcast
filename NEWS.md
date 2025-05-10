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
