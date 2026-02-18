
# broadcast 0.1.8

* Slightly relaxed the behaviour for `NA`s/`NaN`s for the `+` and `-` operators involving complex numbers.
* Streamlined some internal code here and there.
* Changed the internal code to avoid the false-positive issue message in 'Rchk'.
* Built the package with the recently updated version of 'Rcpp'.
* Added the `checkNULL()` and `checkNA()` methods.
* Added a new linear algebra function for statistics: `ecumprob()`.

<br>

# broadcast 0.1.7

**Consistency fixes:**

* In `bc.b()`, in the case of a result with length zero:  
the result will be of type "raw" if both `x` and `y` are raw, and logical otherwise  
(until now, the result was always logical, which is not consistent with the regular case).
* In `bc.d()`:  
the `d==`, `d!=` etc. operators now also accept the case where both `x` and `y` are integer or logical.
* Most methods and operators now also preserve the `comment` attribute.


**Other changes:**

* Added more examples in the reference manual.
* Added more tests.
* Fixed several linguistic errors in the help page of `cast_shallow2atomic()`.

<br>

# broadcast 0.1.6.1

* Replaced all uses of `abs()`/`fabs()` with `std::abs()` in the internal source code.

<br>

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

<br>

# broadcast 0.1.5.3

* Replaced `abs` function with `labs` function when using long integers in src/rcpp_bcFact_int.

<br>

# broadcast 0.1.5.2

* Reduced the installation time and size of the compiled library, with almost no performance loss.

<br>

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

<br>

# broadcast 0.1.3
* Continuation of Initial CRAN submission.
* Fixed the title case.

<br>

# broadcast 0.1.2
* Continuation of Initial CRAN submission.
* Shortened the title.
* Removed the LICENSE file, and its reference in the Description.

<br>

# broadcast 0.1.1
* Continuation of Initial CRAN submission.
* Replaced `abs` function with `labs` function when using long integers in src/rcpp_bc_int.

<br>

# broadcast 0.1
* Initial CRAN submission

<br>

# broadcast 0.0.0.9018
* Small performance improvements (re-ran the benchmarks again).
* Proof-read the documentation, and made some tweaks.
* Started preparations for CRAN release in the near future.

<br>

# broadcast 0.0.0.9000
* Initial GitHub Publication

<br>
