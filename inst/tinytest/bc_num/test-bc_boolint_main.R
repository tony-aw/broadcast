
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


# idiv and mod relationship ====
m <- 31
foo <- expand.grid(c(-m:-1, 1:m), c(-m:-1, 1:m))
i <- foo[[1L]]
j <- foo[[2L]]
x <- sign(i) * (2^abs(i) - 1L)
y <- sign(j) * (2^abs(j) - 1L)
broadcaster(x) <- broadcaster(y) <- TRUE
expect_true(
  all(bc.d(x, (x %% y) + y * (x %/% y), "d=="))
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

