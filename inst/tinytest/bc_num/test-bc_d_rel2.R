
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
types <- c("logical", "integer", "double")


# equals ====
bc.fun <- function(x, y) bc.d(x, y, "==")
base.fun <- function(x, y) {
  as_dbl(x) == as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# unequals ====
bc.fun <- function(x, y) bc.d(x, y, "!=")
base.fun <- function(x, y) {
  as_dbl(x) != as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# smaller ====
bc.fun <- function(x, y) bc.d(x, y, "<")
base.fun <- function(x, y) {
  as_dbl(x) < as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# greater ====
bc.fun <- function(x, y) bc.d(x, y, ">")
base.fun <- function(x, y) {
  as_dbl(x) > as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# se ====
bc.fun <- function(x, y) bc.d(x, y, "<=")
base.fun <- function(x, y) {
  as_dbl(x) <= as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# greater ====
bc.fun <- function(x, y) bc.d(x, y, ">=")
base.fun <- function(x, y) {
  as_dbl(x) >= as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)

