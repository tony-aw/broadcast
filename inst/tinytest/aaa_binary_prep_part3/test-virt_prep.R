
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.binary_prep <- function(x, y) {
  
  x.ndim <- ndim(x)
  y.ndim <- ndim(y)
  out <- broadcast:::.rcpp_virt_binary_prep(x, y, x.ndim, y.ndim)
  names(out) <- c("x.dim", "y.dim", "out.dimorig", "out.dimsimp", "out.len", "dimmode")
  return(out)
  
}


# test unmergeable ====

# vectors:
x <- rnorm(10)
y <- rnorm(10)
out <- .binary_prep(x, y)
expected <- list(
  x.dim = rep(1L, 4), y.dim = rep(1L, 4),
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
  x.dim = rep(1L, 4L), y.dim = rep(1L, 4L),
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
  x.dim = c(dim(x), 1L, 1L), y.dim = c(dim(y), 1L, 1L),
  out.dimorig = out.dim, out.dimsimp = c(out.dim, 1L, 1L), out.len = prod(out.dim),
  dimmode = 2L
)
expect_equal(
  expected, out
)

# big2vector:
x <- array(rnorm(10), c(100L, 90L, 50L))
y <- array(rnorm(10), c(1L, 90L, 1L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(dim(x), 1L), y.dim = c(dim(y), 1L),
  out.dimorig = out.dim, out.dimsimp = c(out.dim, 1L), out.len = prod(out.dim),
  dimmode = 3L
)
expect_equal(
  expected, out
)


# sandwich:
x <- array(rnorm(10), c(100L, 1L, 50L))
y <- array(rnorm(10), c(1L, 90L, 1L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(dim(x), 1L), y.dim = c(dim(y), 1L),
  out.dimorig = out.dim, out.dimsimp = c(out.dim, 1L), out.len = prod(out.dim),
  dimmode = 4L
)
expect_equal(
  expected, out
)



# general (4dim):
x <- array(rnorm(10), c(100L, 90L, 50L, 1L))
y <- array(rnorm(10), c(1L, 90L, 1L, 30L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = dim(x), y.dim = dim(y),
  out.dimorig = out.dim, out.dimsimp = out.dim, out.len = prod(out.dim),
  dimmode = 5L
)
expect_equal(
  expected, out
)


# general (5dim):
x <- array(rnorm(10), c(100L, 90L, 50L, 1L, 20L))
y <- array(rnorm(10), c(1L, 90L, 1L, 30L, 1L))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(dim(x), rep(1L, 11L)), y.dim = c(dim(y), rep(1L, 11L)), 
  out.dimorig = out.dim, out.dimsimp = c(out.dim, rep(1L, 11L)), out.len = prod(out.dim),
  dimmode = 5L
)
expect_equal(
  expected, out
)


enumerate <- enumerate + 7L



# test merge all ====
# 3dim:
x <- array(rnorm(10), c(10, 10, 10))
y <- array(rnorm(10), c(10, 10, 10))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(prod(dim(x)), rep(1L, 3L)), y.dim = c(prod(dim(y)), rep(1L, 3L)),
  out.dimorig = c(10, 10, 10), out.dimsimp = c(1000, rep(1L, 3L)), out.len = 1000,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L

# 4dim:
x <- array(rnorm(10), c(10, 10, 10, 10))
y <- array(rnorm(10), c(10, 10, 10, 10))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(prod(dim(x)), rep(1L, 3L)), y.dim = c(prod(dim(y)), rep(1L, 3L)),
  out.dimorig = c(10, 10, 10, 10), out.dimsimp = c(10000, rep(1L, 3L)), out.len = 10000,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# >4 dim:
for(i in 5:16) {
  x <- array(rnorm(10), rep(2L, i))
  y <- array(rnorm(10), rep(2L, i))
  out <- .binary_prep(x, y)
  expected <- list(
    x.dim = c(prod(dim(x)), rep(1L, 15L)), y.dim = c(prod(dim(y)), rep(1L, 15L)),
    out.dimorig = rep(2L, i), out.dimsimp = c(2^i, rep(1L, 3L)), out.len = 2^i,
    dimmode = 1L
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}


# test drop all ====
# <= 4d:
for(i in 2:4) {
  x <- array(rnorm(1), rep(1L, i))
  y <- array(rnorm(1), rep(1L, i))
  out <- .binary_prep(x, y)
  expected <- list(
    x.dim = rep(1L, 4L), y.dim = rep(1L, 4L),
    out.dimorig = rep(1L, i), out.dimsimp = NULL, out.len = 1,
    dimmode = 1L
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}

# > 4d:
for(i in 5:16) {
  x <- array(rnorm(1), rep(1L, i))
  y <- array(rnorm(1), rep(1L, i))
  out <- .binary_prep(x, y)
  expected <- list(
    x.dim = rep(1L, 16L), y.dim = rep(1L, 16L),
    out.dimorig = rep(1L, i), out.dimsimp = NULL, out.len = 1,
    dimmode = 1L
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}



# test drop ends, merge ins ====
x <- array(rnorm(10), c(1, 10, 10, 10, 1))
y <- array(rnorm(10), c(1, 10, 10, 10, 1))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(1000, rep(1L, 15L)), y.dim = c(1000, rep(1L, 15L)),
  out.dimorig = dim(x), out.dimsimp = c(1000, rep(1L, 3L)), out.len = 1000,
  dimmode = 1L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test merge ends, drop ins ====
x <- array(rnorm(10), c(10, 10, 1, 10, 10))
y <- array(rnorm(10), c(10, 10, 1, 10, 10))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(10000, rep(1, 15L)), y.dim = c(10000, rep(1, 15L)),
  out.dimorig = dim(x), out.dimsimp = c(10000, rep(1, 3L)), out.len = 10000,
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
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(7*8*9, 1L, rep(1L, 14L)), y.dim = c(1, 5*3*4, rep(1L, 14L)),
  out.dimorig = out.dim, out.dimsimp = c(7*8*9, 5*3*4, rep(1L, 2L)), out.len = prod(out.dim),
  dimmode = 2L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L



# test merge big2vector ====
x <- array(rnorm(10L), c(1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 1))
y <- array(rnorm(10L), c(5, 3, 4, 1, 7, 8, 9, 1, 3, 5, 4, 1))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(1L, 7*8*9, 1L, rep(1L, 13L)), y.dim = c(5*3*4, 7*8*9, 3*5*4, rep(1L, 13L)),
  out.dimorig = out.dim, out.dimsimp = c(5*3*4, 7*8*9, 3*5*4, 1L), out.len = prod(out.dim),
  dimmode = 3L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test merge sandwhich ====
x <- array(rnorm(10L), c(1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 1))
y <- array(rnorm(10L), c(5, 3, 4, 1, 1, 1, 1, 1, 3, 5, 4, 1))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(1, 7*8*9, 1, rep(1L, 13L)),
  y.dim = c(5*3*4, 1, 3*5*4, rep(1L, 13L)),
  out.dimorig = out.dim, out.dimsimp = c(5*3*4, 7*8*9, 3*5*4, 1L), out.len = prod(out.dim),
  dimmode = 4L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test merge general ====
x <- array(rnorm(10L), c(1, 1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 5, 6, 7, 1))
y <- array(rnorm(10L), c(1, 5, 3, 4, 1, 1, 1, 1, 1, 3, 5, 4, 1, 1, 1, 1))
out.dim <- pmax(dim(x), dim(y))
out <- .binary_prep(x, y)
expected <- list(
  x.dim = c(1, 7*8*9, 1, 5*6*7, rep(1L, 12L)),
  y.dim = c(5*3*4, 1, 3*5*4, 1, rep(1L, 12L)),
  out.dimorig = out.dim, out.dimsimp = c(5*3*4, 7*8*9, 3*5*4, 5*6*7), out.len = prod(out.dim),
  dimmode = 5L
)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L

