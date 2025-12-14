
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# bc.i ====
x <- as.integer(2^31 - 1)
y <- as.double(x)
expect_equal(
  bc.i(x, 10L, "+"),
  y + 10L
)
expect_equal(
  bc.i(-x, 10L, "-"),
  -y - 10L
)
expect_equal(
  bc.i(x, 10L, "*"),
  y * 10L
)
expect_equal(
  bc.i(x, 1L, "%/%"),
  y %/% 1L
)
expect_equal(
  bc.i(as.integer(x/100L), 2L, "^"),
  as.integer(y/100L) ^ 2L
)

enumerate <- enumerate + 5L


# bc.d ====
x <- as.integer(2^31 - 1)
y <- as.double(x)
expect_equal(
  bc.d(x, x, "+"),
  y + y
)
expect_equal(
  bc.d(-x, x, "-"),
  -y - y
)
expect_equal(
  bc.d(x, x, "*"),
  y * y
)
expect_equal(
  bc.d(x, 0.5, "/"),
  y / 0.5
)
expect_equal(
  bc.d(x, 10L, "^"),
  y ^ 10L
)

enumerate <- enumerate + 5L

