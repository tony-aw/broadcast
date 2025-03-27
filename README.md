
<img src="man/figures/broadcast.png" height="300"/>

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)

<!-- badges: end -->

# Introduction

 

## 🗺️Overview

The ‘broadcast’ ‘R’ - package, as the name suggests, performs
“broadcasting” (similar to broadcasting in the ‘Numpy’ module for
‘Python’).

In the context of operations involving 2 (or more) arrays,
“broadcasting” refers to recycling array dimensions **without**
allocating additional memory, which is considerably **faster** and
**more memory-efficient** than R’s regular dimensions replication
mechanism.

Please read the article “Broadcasting explained” for a more complete
explanation of what “broadcasting” is.

 

At its core, the ‘broadcast’ package provides 3 functionalities, all 3
related to “broadcasting”:

 

First, ‘broadcast’ provides functions for broadcasted element-wise
binary operations between any 2 arrays. They support a large set of
relational-, arithmetic-, Boolean-, and string operations.  
These functions have clear broadcasting rules, making it easy to
accurately predict the dimensions of the result.

 

Second, ‘broadcast’ provides the `bind_array()` function, which is a
broadcasted and enhanced form of the fantastic `abind::abind()`
function:

- `bind_array()` allows for broadcasting (obviously), whereas
  `abind::abind()` does not.
- `bind_array()` is significantly **faster** and uses **less memory**
  than `abind::abind()`.
- `bind_array()` also differs from `abind::abind()` in that it can
  handle recursive arrays properly; `abind::abind()` unlists everything
  to atomic arrays, ruining the structure.

 

Third, ‘broadcast’ provides several generic functions for broadcasting:

- `bcapply()`: a broadcasted apply-like function that works on pairs of
  arrays.
- `bc_ifelse()`: a broadcasted `ifelse()` function. Broadcasts between
  the `yes` and `no` arguments.

 

Additionally, ‘broadcast’ includes the `acast()` function, for
casting/pivoting an array into a new dimension. Roughly analogous to
`data.table::dcast()`, but for arrays.

 

## 🤷🏽Why use ‘broadcast’

**Efficiency**

Broadcasting dimensions is faster and more memory efficient than
replicating dimensions.  
Efficient programs use less energy and resources, and is thus better for
the environment.  
As a favoured language for the sciences, ‘R’ should not throw away an
opportunity to become more efficient.

The `Benchmarks` show that ‘broadcast’ has a somewhat similar speed as
equivalent operations in ‘Numpy’.

 

**Convenience**

Have you ever been bothered by any of the following while programming in
‘R’:

- `abind::abind()` being too slow, saying arrays are not conformable,
  and/or coercing recursive arrays to character arrays?
- that there is no built-in way to cast or pivot arrays?
- Receiving the “non-conformable arrays” error message in a simple
  operation when it intuitively should work?
- Receiving the “Error: cannot allocate vector of size” error message
  because ‘R’ unnecessarily allocates too much memory?
- Trying to perform a simple operation on all possible combinations,
  only to find out you need nested loops and/or grid expansions to do
  something that should be *very* simple?

If you answered “YES” to any of the above, ‘broadcast’ may very well be
the ‘R’ - package for you.

 

**Minimal Dependencies**

Besides linking to ‘Rcpp’, ‘broadcast’ does not depend on, vendor, link
to, include, or otherwise use any external libraries; ‘broadcast’ was
essentially made from scratch and can be installed out-of-the-box.

Not using external libraries brings a number of advantages:

- **Avoid dependency hell**: Every dependency that is added to a
  software package increases the likelihood of something breaking (AKA
  “dependency hell”). ‘broadcast’ thus avoids this.
- **Avoid wasting resources for translations**: Using libraries from
  other languages, such as ‘xtensor’ (‘C++’) or ‘Numpy’ (‘Python’) means
  that - at some point - one needs to convert between the structure of
  ‘R’ to that of the other language, and vice-versa, which wastes
  precious time, memory, and power. ‘broadcast’ requires no such
  translations of structures, and is therefore much less wasteful.
- **Ensure consistent behaviour**: Using libraries from other languages
  also means one cannot always guarantee consistent behaviour for some
  operations. For example: both ‘Numpy’ and ‘xtensor’ have only limited
  support for missing values, whereas ‘R’ supports missing values for
  both atomic and recursive array/vector types (except type of ‘Raw’).
  Since ‘broadcast’ does not rely on external libraries, it can ensure
  behaviour that is consistent with the rest of ‘R’.

 

**Tested**

The ‘broadcast’ package is frequently checked using a large suite of
unit tests via the [tinytest](https://github.com/markvanderloo/tinytest)
package. These tests have a coverage of approximately 95%. As such, the
chance of a function from this package breaking is relatively low.

Since ‘broadcast’ is still relatively new package, bugs are still very
much possible. I encourage users who find bugs to report them swiftly to
the GitHub page, and I will fix them as soon as time permits.

 

## 🚀Quick Example

Consider the matrices `x` and `y`:

``` r
x <- array(1:20, c(4, 5))
y <- array(1:5*10, c(1, 5))
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    5    9   13   17
#> [2,]    2    6   10   14   18
#> [3,]    3    7   11   15   19
#> [4,]    4    8   12   16   20
print(y)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   10   20   30   40   50
```

Suppose one wishes to compute the element-wise addition of these 2
arrays.

This won’t work in base ‘R’:

``` r
x + y
Error in x + y : non-conformable arrays
```

When computing the element-wise sum of these arrays, they both need to
be recycled to equal size in order to compute the element-wise
computation.  
I.e. `y` needs its single row to be recycled 5 times, creating 2
conformable matrices.

You *could* do the following….

``` r
x + y[rep(1L, 4L),]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   11   25   39   53   67
#> [2,]   12   26   40   54   68
#> [3,]   13   27   41   55   69
#> [4,]   14   28   42   56   70
```

… but this involves replicating/copying `y` several times, which costs
memory, reduces speed, and the code is not easily scalable for arrays
with different dimensions.

The ‘broadcast’ package performs “broadcasting”, which can do the above,
but **faster** and **without unnecessary copies**, like so:

``` r
bc.num(x, y, "+")
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   11   25   39   53   67
#> [2,]   12   26   40   54   68
#> [3,]   13   27   41   55   69
#> [4,]   14   28   42   56   70
```

 

## 📖Documentation

The documentation in the ‘broadcast’ website is divided into 3 main
parts:

- Guides and Vignettes: Here you’ll find the topic-oriented guides in
  the form of a few Vignettes.
- Reference Manual: Here you’ll find the function-oriented reference
  manual.
- About: Here you’ll find mainly the Acknowledgements, Change logs and
  License file.

   
