
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
.return_missing <- broadcast:::.return_missing
types <- c("logical", "integer", "int53") # remember that Debian is stupid ...


# plus ====
bc.fun <- function(x, y) bc.i(x, y, "+")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) + as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# min ====
bc.fun <- function(x, y) bc.i(x, y, "-")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) - as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# multiply ====
bc.fun <- function(x, y) bc.i(x, y, "*")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) * as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# power ====
bc.fun <- function(x, y) bc.i(x, y, "^")
base.fun <- function(x, y) {
  x <- as_dbl(trunc(x))
  y <- as_dbl(trunc(y))
  out <- trunc(x^y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# pmin ====
bc.fun <- function(x, y) bc.i(x, y, "pmin")
base.fun <- function(x, y) {
  pmin(as_dbl(trunc(x)), as_dbl(trunc(y)))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# pmax====
bc.fun <- function(x, y) bc.i(x, y, "pmax")
base.fun <- function(x, y) {
  pmax(as_dbl(trunc(x)), as_dbl(trunc(y)))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# attributes tests ====
bc.fun <- function(x, y) { bc.i(x, y, "+")}

res <- .test_binary_class(bc.fun, types, types)
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
expect_equal(
  res$expected_ma, res$out_ma
)
enumerate <- enumerate + res$i


# zerolen tests ====
bc.fun <- function(x, y) { bc.i(x, y, "+")}
res <- .test_binary_zerolen(bc.fun, is.double, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i



