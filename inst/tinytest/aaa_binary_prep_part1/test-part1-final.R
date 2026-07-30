

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
enumerate <- 0L

testfun <- function(x, y) {
  x.ndim <- ndim(x)
  y.ndim <- ndim(y)
  return(broadcast:::.rcpp_virt_part1_test(x, y, x.ndim, y.ndim))
}

# vectors ====
x <- 1:10
y <- 1:10

expect_equal(
  testfun(x, y)[[3L]],
  NULL
)

expect_equal(
  testfun(x, y)[[4L]],
  10L
)

enumerate <- enumerate + 2L


# vector x array ====
x <- 1:10
y <- array(1:10, c(1, 10))

expect_equal(
  testfun(x, y)[[3L]],
  c(10L, 10L)
)

expect_equal(
  testfun(x, y)[[4L]],
  100L
)

enumerate <- enumerate + 2L


# array x vector ====
y <- 1:10
x <- array(1:10, c(1, 10))

expect_equal(
  testfun(x, y)[[3L]],
  c(10L, 10L)
)

expect_equal(
  testfun(x, y)[[4L]],
  100L
)

enumerate <- enumerate + 2L


# arrays ====
x <- array(1:10, c(5, 1, 7))
y <- array(1:10, c(1, 10, 7))

expect_equal(
  testfun(x, y)[[3L]],
  c(5L, 10L, 7L)
)

expect_equal(
  testfun(x, y)[[4L]],
  350L
)

enumerate <- enumerate + 2L


# arrays - full orthogonal ====
x <- array(1:10, c(5, 1, 7))
y <- array(1:10, c(1, 10, 1))

expect_equal(
  testfun(x, y)[[3L]],
  c(5L, 10L, 7L)
)

expect_equal(
  testfun(x, y)[[4L]],
  350L
)

enumerate <- enumerate + 2L



# big x small ====
x <- array(1:10, c(5, 1, 7, 1))
y <- array(1:10, c(1, 10, 1))

expect_equal(
  testfun(x, y)[[3L]],
  c(5L, 10L, 7L, 1L)
)

expect_equal(
  testfun(x, y)[[4L]],
  350L
)

enumerate <- enumerate + 2L



# small x big ====
y <- array(1:10, c(5, 1, 7, 1))
x <- array(1:10, c(1, 10, 1))

expect_equal(
  testfun(x, y)[[4L]],
  350L
)

enumerate <- enumerate + 2L

