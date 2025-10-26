
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
types <- c("logical", "integer", "int53")

# equals ====
bc.fun <- function(x, y) bc.i(x, y, "==")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) == as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# unequal ====
bc.fun <- function(x, y) bc.i(x, y, "!=")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) != as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# smaller ====
bc.fun <- function(x, y) bc.i(x, y, "<")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) < as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# greater ====
bc.fun <- function(x, y) bc.i(x, y, ">")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) > as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# se ====
bc.fun <- function(x, y) bc.i(x, y, "<=")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) <= as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# ge ====
bc.fun <- function(x, y) bc.i(x, y, ">=")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) >= as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)

