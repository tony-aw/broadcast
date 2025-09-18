
<p align="center">
<img src="man/figures/broadcast.png" height="300"/>
</p>
<p align="center">
‘R’-package: Broadcasted Array Operations Like ‘NumPy’
</p>
<!-- badges: start -->

[![R build
status](https://github.com/tony-aw/broadcast/workflows/R-CMD-check/badge.svg)](https://github.com/tony-aw/broadcast/actions)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![](https://img.shields.io/badge/Author-Tony%20Wilkes-red.svg)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
[![](https://www.r-pkg.org/badges/version/broadcast)](https://cran.r-project.org/package=broadcast)
[![Dependencies](https://tinyverse.netlify.app/badge/broadcast)](https://cran.r-project.org/package=broadcast)  
<!-- badges: end -->

 

## 🗺️Overview

‘broadcast’ is an efficient ‘C’/‘C++’ - based ‘R’ package that, as the
name suggests, performs “broadcasting” (similar to broadcasting in the
‘Numpy’ module for ‘Python’).

In the context of operations involving 2 (or more) arrays,
“broadcasting” refers to recycling array dimensions **without**
allocating additional memory, which is considerably **faster** and
**more memory-efficient** than R’s regular dimensions replication
mechanism.

Please read the [Broadcasting
explained](https://tony-aw.github.io/broadcast/vignettes/d_broadcasting_explained.html)
page for more details on “broadcasting”.

 

At its core, the ‘broadcast’ package provides the following
functionalities, all related to “broadcasting”:

1.  Broadcasted Infix Operators.  
    They support a large set of relational-, arithmetic-, Boolean-,
    string-, and bit-wise operations.
2.  The `bind_array()` function for binding arrays along any arbitrary
    dimension. Similar to the fantastic `abind::abind()` function, but
    with a few key differences:
    - `bind_array()` is faster and more memory efficient;
    - `bind_array()` supports broadcasting;
    - `bind_array()` supports both atomic and recursive arrays
      (`abind()` only supports atomic arrays).
3.  ‘broadcast’ provides several generic functions for broadcasting,
    namely `bcapply()` (broadcasted apply-like function) and
    `bc_ifelse()` (broadcasted version of `ifelse()`).
4.  casting functions, that cast subset-groups of an array to a new
    dimension, cast nested lists to dimensional lists, and vice-versa.  
    These functions are useful for facilitating complex broadcasted
    operations, though they also have much merit beside broadcasting.

Additionally, ‘broadcast’ comes with a few linear algebra functions for
statistics.

 

## 🤷🏽Why use ‘broadcast’

**Efficiency**

Broadcasting dimensions is faster and more memory efficient than
replicating dimensions.  
Efficient programs use less energy and resources, and is thus better for
the environment.  
Benchmarks can be found in the “About” section on the website.

 

**Convenience**

Have you ever been bothered by any of the following while programming in
‘R’:

- Receiving the “non-conformable arrays” error message in a simple array
  operation, when it intuitively should work?
- Receiving the “cannot allocate vector of size…” error message because
  ‘R’ unnecessarily allocated too much memory in array operations?
- `abind::abind()` being too slow, or ruining the structure of recursive
  arrays?
- that there is no array analogy to `data.table::dcast()`?
- difficulties in handling nested lists?
- that certain ‘Numpy’ operations have no equivalent operation in ‘R’?

If you answered “YES” to any of the above, ‘broadcast’ may be the ‘R’ -
package for you.

 

**Minimal Dependencies**

Besides linking to ‘Rcpp’, ‘broadcast’ does not depend on, vendor, link
to, include, or otherwise use any external libraries; ‘broadcast’ was
essentially made from scratch and can be installed out-of-the-box.

Not using external libraries brings a number of advantages:

- Avoid dependency hell.
- Avoid wasting time, memory and computing resources for translating
  between language structures.
- Ensure consistent behaviour with the rest of ‘R’.

 

**Tested**

The ‘broadcast’ package is frequently checked using a large suite of
unit tests via the [tinytest](https://github.com/markvanderloo/tinytest)
package. These tests have a coverage of over 90%. So the chance of a
function from this package breaking completely is relatively low.

‘broadcast’ is still relatively new package, however, so (small) bugs
are still very much possible. I encourage users who find bugs to report
them promptly to the
[issues](https://github.com/tony-aw/broadcast/issues) tab on the GitHub
page, and I will fix them as soon as time permits.

 

## 🚀Quick Example

Consider the matrices `x` and `y`:

``` r
x <- array(1:20, c(4, 5))
y <- array(1:5 * 100, c(1, 5))
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    5    9   13   17
#> [2,]    2    6   10   14   18
#> [3,]    3    7   11   15   19
#> [4,]    4    8   12   16   20
print(y)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  100  200  300  400  500
```

Suppose one wishes to compute the element-wise addition of these 2
arrays.

This won’t work in base ‘R’:

``` r
x + y
Error in x + y : non-conformable arrays
```

You *could* do the following….

``` r
x + y[rep(1L, 4L),]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  101  205  309  413  517
#> [2,]  102  206  310  414  518
#> [3,]  103  207  311  415  519
#> [4,]  104  208  312  416  520
```

… but this becomes an issue when `x` and/or `y` become very large, as
the above operation involves replicating `y` several times - which costs
memory, reduces speed, and the code is not easily scalable for arrays
with different dimensions.

The ‘broadcast’ package performs “broadcasting”, which can do the above,
but **faster**, **without unnecessary copies**, and scalable to arrays
of any size (up to 16 dimensions).

Like so:

``` r

broadcaster(x) <- TRUE
broadcaster(y) <- TRUE

x + y
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  101  205  309  413  517
#> [2,]  102  206  310  414  518
#> [3,]  103  207  311  415  519
#> [4,]  104  208  312  416  520
#> broadcaster
```

 

## 📊Status

‘broadcast’ is now available on CRAN! 🎉

If you have any suggestions or feedback on the package, its
documentation, or even the benchmarks, I encourage you to let me know
(either as an [Issue](https://github.com/tony-aw/broadcast/issues) or a
[Discussion](https://github.com/tony-aw/broadcast/discussions)).  
I’m eager to read your input!

 

## 📖Documentation

The documentation in the ‘broadcast’ website is divided into 3 main
parts:

- [Guides and
  Vignettes](https://tony-aw.github.io/broadcast/vignettes/a_readme.html):
  contains the topic-oriented guides in the form of a few Vignettes.
- [Reference
  Manual](https://tony-aw.github.io/broadcast/man/aaa00_broadcast_help.html):
  contains the function-oriented reference manual.
- [About](https://tony-aw.github.io/broadcast/about/a_acknowledgements.html):
  Contains the Acknowledgements, Change logs and License file. Here
  you’ll also find some information regarding the relationship between
  ‘broadcast’ and other packages/modules. Benchmarks can also be found
  here.

   
