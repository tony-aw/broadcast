
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
types <- c("logical", "integer", "int53") # remember that Debian is stupid ...


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



# attributes tests ====
bc.fun <- function(x, y) { bc.d(x, y, "==")}

res <- .test_binary_class(bc.fun, types, types)
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
expect_false(
  identical(res$expected_ma, res$out_ma)
)
enumerate <- enumerate + res$i


# zerolen tests ====
bc.fun <- function(x, y) { bc.d(x, y, "==")}
res <- .test_binary_zerolen(bc.fun, is.logical, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
enumerate <- enumerate + res$i

