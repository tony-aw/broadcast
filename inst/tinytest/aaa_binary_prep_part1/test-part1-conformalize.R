

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
enumerate <- 0L

testfun <- function(x, y) {
  x.ndim <- ndim(x)
  y.ndim <- ndim(y)
  out <- broadcast:::.rcpp_virt_part1_test(x, y, x.ndim, y.ndim)
  return(out)
}

# x is length zero, y is not ====
x <- array(integer(0L))
y <- array(1:10, sample(2:10, 4L, TRUE))
expect_error(
  testfun(x, y),
  "arrays not conformable"
)

# y is length zero, x is not ====
x <- array(1:10, sample(2:10, 4L, TRUE))
y <- array(integer(0L))
expect_error(
  testfun(x, y),
  "arrays not conformable"
)

# y is dimensional, x is not ====
for(iLen in 1:10) {
  
  x <- 1:iLen
  y <- array(1:10, c(iLen, rep(1L, 3L)))
  testout <- testfun(x, y)
  
  x.dim <- testout[[1L]]
  y.dim <- testout[[2L]]
  
  expect_equal(
    y.dim,
    c(iLen, rep(1L, 3L))
  ) |> errorfun()
  
  expect_equal(
    x.dim,
    c(iLen, rep(1L, 3L))
  ) |> errorfun()
  
  
  enumerate <- enumerate + 1L
  
}


# x is dimensional, y is not ====
for(iLen in 1:10) {
  
  x <- array(1:10, c(iLen, rep(1L, 3L)))
  y <- 1:iLen
  
  testout <- testfun(x, y)
  
  x.dim <- testout[[1L]]
  y.dim <- testout[[2L]]
  
  expect_equal(
    y.dim,
    c(iLen, rep(1L, 3L))
  ) |> errorfun()
  
  expect_equal(
    x.dim,
    c(iLen, rep(1L, 3L))
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}

# x has few dimensions, y has many dimensions ====
x <- array(1:10, c(1L, 5L))
y <- array(1:10, c(5L, rep(1L, 4L), 5L))

testout <- testfun(x, y)
x.dim <- testout[[1L]]
y.dim <- testout[[2L]]
expect_equal(
  x.dim,
  c(dim(x), rep(1L, 16L - ndim(x)))
)
expect_equal(
  y.dim,
  c(dim(y), rep(1L, 16L - ndim(y)))
)


# y has few dimensions, x has many dimensions ====
y <- array(1:10, c(1L, 5L))
x <- array(1:10, c(5L, rep(1L, 4L), 5L))

testout <- testfun(x, y)
x.dim <- testout[[1L]]
y.dim <- testout[[2L]]

expect_equal(
  y.dim,
  c(dim(y), rep(1L, 16L - ndim(y)))
)
expect_equal(
  y.dim,
  c(dim(y), rep(1L, 16L - ndim(y)))
)

