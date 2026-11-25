Functional Technical Design
================
Tony Wilkes

# Introduction

This is document does not represent user-oriented documentation for the
‘broadcast’ package. I simply use this document to report my design
choices, primarily in case I ever wonder why I did one thing instead of
another.

 

# Mixing types

In general, the input of binary operations are coerced to the same type
before sending it to the ‘C’/‘C++’ code. The only exception are the
numeric relops.

 

# Operation Family Methods

‘broadcast’ supports all 6 atomic type arrays and recursive arrays
(arrays of type `list`). Each of these types are associated with a
family of operations. For example, `logical` is associated with `&, |`.
But notice that `&,|` have a different meaning for type `raw`: they are
bit-wise operations for `raw`.

Let’s look at bit-wise operators `&` and `|`. For type `raw`, they
perform bit-wise operations, but for integers they perform logical
operations. but what if you want to perform bit-wise operations on
integers? ‘R’ provides the `bitw*` functions for that. But what if you
want to perform logical operations on `raw`? You’d have to convert them
to logical (wasting 4 times the memory!), perform logical operations,
and convert back. This is sloppy and wastes memory.

With that in mind, instead of making a separate method for each symbol
(like “+”), I made a separate method for each family of operations. With
the caveat that these method work consistently for different types. I.e.
logical families always perform logical operations, not logical for
`logical` and bit-wise for `raw`.

These family-oriented methods have at least main operators, but
sometimes also relational and distance operations. I’ll discuss those
further down below.

So we get the following methods:

- `bc.rel()`: relational operations. Supports all atomic types.
- `bc.b()`: logical operations. Supports logical, integer, and `raw`.
- `bc.i()`: integer operations. Also includes rounded relational
  operations to prevent wasting memory if you just want to check if 2
  rounded values are equal.
- `bc.d()`: arithmetic operations. Supports, like base ‘R’, logical,
  integer, and numeric types. Since numerics are the main **ordered**
  type, this one also includes the main relational operations
  (`bc.rel()` uses the same rcpp script as `bc.d()` for greater
  than/smaller than), as well as the `%d==&` etc. operators from my
  ‘tinycodet’ package.
- `bc.cplx()`: similar to `bc.d()`, but for complex numbers. Note that
  there is no universally agreed upon definition for “raise to power”
  for complex numbers, and complex numbers have no real ordering. Thus
  there are no “^” and no greater/smaller than operations.
- `bc.str()`: string operations.
- `bc.bit()`: bit-wise operations. Supports `raw` and `integer`
  operations.
- `bc.list()`: apply any ‘R’ function to an array. The main difference
  with `bcapply()` is that `bc.list()` takes as input list and as output
  list, and can therefore be faster than the more generic `bcapply()`.

 

## Considerations on relops in bit-wise vs logical operations

in ‘broadcast’, greater/smaller than operations for bit-wise operations
are supported. For example, `!x & y` and `!x | y` are exactly equal to
**bit-wise** `x < y`, and `x <= y`, respectively.

This does not hold for logicals, because `NA` is handled differently in
`&` and `|` then in relops. Moreover, stricly speaking, “true” and
“false” have no order except when coerced to numbers. Bits, however, are
literally 0s and 1s. So, for both pragmatic and theoretical reasons,
`bc.b()` does not support greater/smaller than-like relops, unlike
`bc.bit()`.

As a side note, both `bc.b()` and `bc.bit()` support “nand” and “nor”.
This is because, just like `&` and `|`, `!` has a bit-wise meaning for
raw and a logical meaning for logical/integer. But, the nice thing is
that `nand(x,x)` is the same as negating `x`. So `bc.b(x,x, "nand")` is
equivalent to logical negation regardless of the type of `x`, and
`bc.bit(x, x, "nand")` is equivalent to bit-wise negation regarless of
the type of `x`. The same goes for “nor”.

 

## Considerations on integer operations

‘R’ supports floating numbers for `%%` and `%/%`. I really don’t like
this. For a couple of reasons.

First, `%%` represents a measure divisibility. Sure, you could say that
`5/2.5` is `2`. But is there really a `2.5` at all? Maybe it’s
`2.50000000001`. And then suddenly the answer is `1`. You don’t know if
`2.50000000001` is supposed to be that number, or if there is a rounding
error.

Even when talking about degrees and hours, in 99% of the cases, you’re
still measuring in somewhat discrete way (like in seconds instead of
hours, or discrete ticks of `.1` degrees instead of whole degrees). It
is far too sloppy for me to use actual fractional numbers, so I flat out
refuse to support it.

Instead, I treat integer operations exactly as they are: actual integers
operations.

‘R’ supports 53 bit integers through doubles. Base ‘R’, as well as ‘C’
and ‘C++’ typically convert doubles to integers, R_xlen_t or long long
by truncating. In line with that, truncation of `double` was also the
original design of ‘broadcast’.

For example:

``` r
x <- 1:10
ind <- 1 - .Machine$double.eps
x[ind] # gives integer(0), because trunc(0.999999999) == 0
```

    ## integer(0)

I did consider the issue of truncating `4.999999999` (which is
essentially `5`) to `5` by nudging the number to truncate 2 ULP away
from zero (provided this nudging was `< 0.5`), but I found this to be a
bit too sloppy.

So with version `0.2.0` I decided to break that decision and make the
(IMHO) more sane choice of using rounding.

To allow the user to also round like `bc.i()` does, `round2int53()` has
been included.

Another consideration is the internal function definition of the `%%`
and `%/%` operators. In base ‘R’ they are defined using a `long double`,
which in Mac OS (the ever left-out OS) is just defined as a regular
`double`. Being as stubborn as I am, I choose to define my functions
based on `long long`, which is a type present across all major OS -
including Mac OS. Of course, the definition was made in such a way that
the numbers correspond to what ‘R’ itself produces for the given
operators.

Base ‘R’ only supports the integer operations `%%` and `%/%`. I added
the oddly missing GCD (greatest common divisor) operator. LCM (least
common multiple) was not included, as that can easily be calculated by
combining GCD with arithmetic. In fact, several integer operations can
be defined in terms of GCD, making it all the more weird it’s not
natively present in base ‘R’.

 

## Considerations on arithmetic operations

The `*, /, ^` operators in ‘broadcast’ always return a type of `double`.
`+, -` will return a type of `integer` if both sides of the argument are
integer/logical. When overflow occurs, and error is returned. For 2
reasons: (1) the code is faster that way, and (2) if you have overflow
the user probably doesn’t want to wait any second longer for the faulty
computation to be complete.

 

## Considerations for string operations

I know I can do too much sometimes, so I’ve put some strict rules on
what string operations I will and will not support.

There are currently just these simple rules:

- all operations are between character arrays/vectors. So not character
  by integer operations (hence I removed `bc_strrep()`).
- all string distance operations, if provided, should be true metrics
  (hence I removed “lcss”). So no OSA distance either.
- no ICU-like pattern detection operations (that is definitely beyond
  the scope of ‘broadcast’).
- no greater than/smaller than operations (‘R’ supports them, but I
  won’t).

 

# General Methods

2 more general methods are supported by ‘broadcast’: `bc_ifelse()` and
`bcapply()`. There are no plans to include additional general methods.

 

# Operator Overloading

I don’t like making custom classes. I created the ‘broadcaster’ class
for only these reasons:

- overloads the arithmetic operators so that operator precedence works
  properly
- uses R’s own dispatch system; with the ‘broadcaster’ class being put
  last, other methods will “win” from the broadcaster methods.
- less overhead than using the S4 system.

I plan on making a better alternative (without removing the option to
still use the broadcaster class of course), but for now this will have
to do.

 

# Insights from benchmarks

A collection of insights I’ve gotten from benchmarking:

- the 1-vector mode gives no performance improvement for “heavy” types
  like `double`. Presumably because doing arithmetic on doubles as well
  as reading/writing `double` values takes more time than you’d gain
  from the extra performance from the 1-vector mode. For int-like
  operations though, the 1-vector mode gives a lot of speed improvements
- `std::isnan()` is way faster than `R_isnancpp()`, and similarly
  `std::isfinite()` is faster than `R_FINITE()`.
- branchless “OR” is faster than short-circuited “OR” if the
  computations on either side of the “OR” is a simple computations like
  `px[i] == NA_INTEGER`.
- The new odometer approach is at least as fast as, and for int-like
  operations even faster than - the deeply nested loop approach.
- The `__restrict__` keyword (with the necessary OS dependent checks in
  the header, of course) gives no performance improvement for most
  broadcasted operations.
- The `pragma omp` keyword (with the necessary OS dependent checks in
  the header, of course) is very limiting, and gives barely 5%
  improvement for double operations. It cannot be used for int-like
  operations as the keyword cannot handle branched code.
- For atomic types, allocating them in ‘Rcpp’ with
  `<Type>Vector(no_init(n))` is faster than just `<Type>Vector(n)`. But
  for lists, there is no observable speed or memory difference between
  `List(no_init(n))` and `List(n)`.
