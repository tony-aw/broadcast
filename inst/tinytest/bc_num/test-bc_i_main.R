
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




# mod ====
bc.fun <- function(x, y) bc.i(x, y, "%%")
base.fun <- function(x, y) {
  as_dbl(trunc(x)) %% as_dbl(trunc(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# idiv - special values ====
values <- c(Inf, -Inf, NA, NaN, 0, -1, 1, 10, -10)
for(i in values) {
  for(j in values) {
    expect_equal(
      bc.i(i, j, "%/%"),
      i %/% j
    ) |> errorfun()
  }
}


# idiv (53bit) ====
bc.fun <- function(x, y) bc.i(x, y, "%/%")
base.fun <- function(x, y) {
  if(is.double(x)) x <- trunc(x)
  if(is.double(y)) y <- trunc(y)
  out <- x %/% y
  return(out)
}

res <- .test_binary(bc.fun, base.fun, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i # count number of tests



# idiv and mod relationship ====
m <- 52
foo <- expand.grid(c(-m:-1, 1:m), c(-m:-1, 1:m))
i <- foo[[1L]]
j <- foo[[2L]]
x <- sign(i) * (2^abs(i) - 1L)
y <- sign(j) * (2^abs(j) - 1L)
broadcaster(x) <- broadcaster(y) <- TRUE
expect_true(
  all(bc.d(x, (x %% y) + y * (x %/% y), "d=="))
)

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
res <- .test_binary_zerolen(bc.fun, \(x)is.double(x)|is.integer(x), "double", "double")
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i


bc.fun <- function(x, y) { bc.i(x, y, "%%")}
res <- .test_binary_zerolen(bc.fun, \(x)is.double(x)|is.integer(x), "integer", "double")
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i

bc.fun <- function(x, y) { bc.i(x, y, "%%")}
res <- .test_binary_zerolen(bc.fun, \(x)is.double(x)|is.integer(x), "double", "integer")
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i

