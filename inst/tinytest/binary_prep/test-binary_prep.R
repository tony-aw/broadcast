
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.binary_prep <- broadcast:::.binary_prep


# test unmergeable ====

# vectors:
x <- rnorm(10)
y <- rnorm(10)
out <- .binary_prep(x, y)
expected <- list(
  x.dim = NULL, y.dim = NULL,
  out.dimorig = NULL, out.dimsimp = NULL, out.len = 10L,
  dimmode = 1
)
expect_equal(
  expected, out
)

# undim:
x <- array(rnorm(10), 10L)
y <- array(rnorm(10), 10L)
out <- .binary_prep(x, y)
expected <- list(
  x.dim = NULL, y.dim = NULL,
  out.dimorig = 10L, out.dimsimp = NULL, out.len = 10L,
  dimmode = 1
)
expect_equal(
  expected, out
)

# ortho:
x <- array(rnorm(10), c(100L, 1L))
y <- array(rnorm(10), c(1L, 90L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = dim(x),
  y.dim = dim(y),
  out.dimorig = out.dim, out.dimsimp = out.dim, out.len = prod(out.dim),
  dimmode = 2L
)
expect_equal(
  expected, out
)

# sandwhich:
x <- array(rnorm(10), c(100L, 90L, 50L))
y <- array(rnorm(10), c(1L, 90L, 1L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = dim(x),
  y.dim = dim(y),
  out.dimorig = out.dim, out.dimsimp = out.dim, out.len = prod(out.dim),
  dimmode = 3L
)
expect_equal(
  expected, out
)

# general:
x <- array(rnorm(10), c(100L, 90L, 50L, 1L))
y <- array(rnorm(10), c(1L, 90L, 1L, 30L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = dim(x),
  y.dim = dim(y),
  out.dimorig = out.dim, out.dimsimp = out.dim, out.len = prod(out.dim),
  dimmode = 4L
)
expect_equal(
  expected, out
)


enumerate <- enumerate + 5L



# test merge all ====
x <- array(rnorm(10), c(10, 10, 10))
y <- array(rnorm(10), c(10, 10, 10))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = prod(dim(x)),
  y.dim = prod(dim(y)),
  out.dimorig = c(10, 10, 10), out.dimsimp = 1000, out.len = 1000,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test drop all ====
x <- array(rnorm(1), rep(1L, 5L))
y <- array(rnorm(1), rep(1L, 5L))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = NULL,
  y.dim = NULL,
  out.dimorig = rep(1L, 5L), out.dimsimp = NULL, out.len = 1,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test drop ends, merge ins ====
x <- array(rnorm(10), c(1, 10, 10, 10, 1))
y <- array(rnorm(10), c(1, 10, 10, 10, 1))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = 1000,
  y.dim = 1000,
  out.dimorig = dim(x), out.dimsimp = 1000, out.len = 1000,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test merge ends, drop ins ====
x <- array(rnorm(10), c(10, 10, 1, 10, 10))
y <- array(rnorm(10), c(10, 10, 1, 10, 10))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = 10000,
  y.dim = 10000,
  out.dimorig = dim(x), out.dimsimp = 10000, out.len = 10000,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L



# test merge ortho ====
x <- array(rnorm(10), c(7, 8, 1, 9, 1, 1, 1, 1))
y <- array(rnorm(10), c(1, 1, 1, 1, 5, 3, 4, 1))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = c(7*8*9, 1L),
  y.dim = c(1, 5*3*4),
  out.dimorig = out.dim, out.dimsimp = c(7*8*9, 5*3*4), out.len = prod(out.dim),
  dimmode = 2L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L



# test merge sandwich ====
x <- array(rnorm(10L), c(1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 1))
y <- array(rnorm(10L), c(5, 3, 4, 1, 7, 8, 9, 1, 3, 5, 4, 1))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = c(1, 7*8*9, 1),
  y.dim = c(5*3*4, 7*8*9, 3*5*4),
  out.dimorig = out.dim, out.dimsimp = c(5*3*4, 7*8*9, 3*5*4), out.len = prod(out.dim),
  dimmode = 3L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L



# test merge general ====
x <- array(rnorm(10L), c(1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 1))
y <- array(rnorm(10L), c(5, 3, 4, 1, 1, 1, 1, 1, 3, 5, 4, 1))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y, sys.call())
expected <- list(
  x.dim = c(1, 7*8*9, 1, 1),
  y.dim = c(5*3*4, 1, 3*5*4, 1),
  out.dimorig = out.dim, out.dimsimp = c(5*3*4, 7*8*9, 3*5*4, 1), out.len = prod(out.dim),
  dimmode = 4L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L
