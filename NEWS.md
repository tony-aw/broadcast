
# broadcast 0.1.6.1

* Replaced all uses of `abs()`/`fabs()` with `std::abs()` in the internal source code.


# broadcast 0.1.6

* **Bug fix:** There was a bug in `acast()`, where it would, under certain circumstances, incorrectly specify `dimnames` to the output, leading to an error. This is now fixed.
* Small speed improvement for nearly all operations.
* The `acast()` method now allows unequal groups even when `x` is of type `raw`.
* Added the `cast_shallow2atomic()` casting method.
* The `cast_hier2dim()` method can now set `dimnames` automatically by specifying the new `direction.names` argument.
* Added the `bc_strrep()` method.
* Added the `vector2array()` and `undim()` helper functions.
* Added `bcr` as short-hand for `broadcaster`.
* Added `mbroadcasters()`.
* If one of the input arrays in `bind_array()` is a `broadcaster`, then the result will also be a `broadcaster`.
* Added more tests.


# broadcast 0.1.5.3

* Replaced `abs` function with `labs` function when using long integers in src/rcpp_bcFact_int.


# broadcast 0.1.5.2

* Reduced the installation time and size of the compiled library, with almost no performance loss.


# broadcast 0.1.5

Update of first CRAN release.

**Argument Changes:**

The `recurse_classed` argument in the casting methods has been replaced with the `recurse_all` argument.  
Before, the argument `recurse_classed` controlled if the casting methods recurse through classed lists.  
Now, `recurse_all`, controls if the casting methods recurse through classed **and/or dimensional** lists.

Moreover, the S3 methods in this package now check for unknown arguments given through the ellipsis (`...`).

**Behavioural Changes:**

* The `as_*` functions now also preserve the `broadcaster` class attribute. 
* If both `x` and `y` in the `bc.b()` method are of type `raw`, `bc.b()` will return type of `raw`.

**New Methods:**

* Added the `hiernames2dimnames()` method, to make it easier to compose `dimnames` for the result of `cast_hier2dim()`.


**Documentation Improvements**:

* Added on-attach package start-up message.
* Fixed some spelling errors that went under the radar.
* Fixed some inconsistent usage of Title Case in the titles of the help pages.
* Shortened the main help page.
* Fixed some "See Also" sections in some of the help pages.
* Improved the "Examples" sections in some of the help pages.

**More tests:**

* Added and adapted the unit tests for the above changes.
* Re-ran the unit tests coverage report shown on the website.



# broadcast 0.1.3
* Continuation of Initial CRAN submission.
* Fixed the title case.


# broadcast 0.1.2
* Continuation of Initial CRAN submission.
* Shortened the title.
* Removed the LICENSE file, and its reference in the Description.


# broadcast 0.1.1
* Continuation of Initial CRAN submission.
* Replaced `abs` function with `labs` function when using long integers in src/rcpp_bc_int.


# broadcast 0.1
* Initial CRAN submission


# broadcast 0.0.0.9018
* Small performance improvements (re-ran the benchmarks again).
* Proof-read the documentation, and made some tweaks.
* Started preparations for CRAN release in the near future.


# broadcast 0.0.0.9000
* Initial GitHub Publication
