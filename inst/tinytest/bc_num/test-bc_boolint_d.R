
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
.return_missing <- broadcast:::.return_missing
types <- c("logical", "integer") # remember that Debian is stupid ...




# mod ====
bc.fun <- function(x, y) bc.i(x, y, "%%")
base.fun <- function(x, y) {
  x %% y
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# idiv (32 bit) ====
bc.fun <- function(x, y) bc.i(x, y, "%/%")
base.fun <- function(x, y) {
  out <- x %/% y
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
  pmin(x, y)
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
  pmax(x, y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# int bit tests ====
bc.fun <- function(x, y) bc.i(x, y, "%%")
base.fun <- function(x, y) {
  x %% y
}

res <- .test_binary(bc.fun, base.fun, types, types)
res_is.integer <- vapply(res$out, is.integer, logical(1L))
expect_true(all(res_is.integer))


# attributes tests ====
bc.fun <- function(x, y) { bc.i(x, y, "%%")}

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
bc.fun <- function(x, y) { bc.i(x, y, "%%")}
res <- .test_binary_zerolen(bc.fun, is.integer, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i

