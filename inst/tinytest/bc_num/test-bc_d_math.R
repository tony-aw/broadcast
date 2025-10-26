
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
types <- c("logical", "integer", "double")


# plus ====
bc.fun <- function(x, y) bc.d(x, y, "+")
base.fun <- function(x, y) {
  as_dbl(x) + as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# minus ====
bc.fun <- function(x, y) bc.d(x, y, "-")
base.fun <- function(x, y) {
  as_dbl(x) - as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# multiply ====
bc.fun <- function(x, y) bc.d(x, y, "*")
base.fun <- function(x, y) {
  as_dbl(x) * as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# divide ====
bc.fun <- function(x, y) bc.d(x, y, "/")
base.fun <- function(x, y) {
  as_dbl(x) / as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)





# power ====
bc.fun <- function(x, y) bc.d(x, y, "^")
base.fun <- function(x, y) {
  as_dbl(x) ^ as_dbl(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# pmin ====
bc.fun <- function(x, y) bc.d(x, y, "pmin")
base.fun <- function(x, y) {
  pmin(as_dbl(x), as_dbl(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)





# pmax ====
bc.fun <- function(x, y) bc.d(x, y, "pmax")
base.fun <- function(x, y) {
  pmax(as_dbl(x), as_dbl(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




