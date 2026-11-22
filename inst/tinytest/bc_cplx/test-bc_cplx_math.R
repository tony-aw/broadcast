
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
types <- "complex"


# basic tests ====
powers <- 2^c(0:6)
values <- sample(c(rnorm(5), 2^powers -1, -2^powers + 1, 1, 0, -1, NA, NaN, Inf, -Inf))
foo <- expand.grid(xr = values, xi = values, yr = values, yi = values)
x <- foo[[1]] + foo[[2]] * 1i
y <- foo[[3]] + foo[[4]] * 1i
expect_equal(
  bc.cplx(x, y, "+") |> as.vector(),
  as.vector(x + y)
)
expect_equal(
  bc.cplx(x, y, "-") |> as.vector(),
  as.vector(x - y)
)
expect_equal(
  bc.cplx(x, y, "*") |> as.vector(),
  as.vector(x * y)
)
expect_equal(
  bc.cplx(x, y, "/") |> as.vector(),
  as.vector(x / y)
)

enumerate <- enumerate + 4L



# plus ====
bc.fun <- function(x, y) bc.cplx(x, y, "+")
base.fun <- function(x, y) {
  as_cplx(x) + as_cplx(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# min ====
bc.fun <- function(x, y) bc.cplx(x, y, "-")
base.fun <- function(x, y) {
  as_cplx(x) - as_cplx(y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# multiply ====
bc.fun <- function(x, y) bc.cplx(x, y, "*")
base.fun <- function(x, y) {
  out <- as_cplx(x) * as_cplx(y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# div ====
bc.fun <- function(x, y) bc.cplx(x, y, "/")
base.fun <- function(x, y) {
  out <- as_cplx(x) / as_cplx(y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# attributes tests ====
bc.fun <- function(x, y) { bc.cplx(x, y, "+")}
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
bc.fun <- function(x, y) { bc.cplx(x, y, "+")}
res <- .test_binary_zerolen(bc.fun, is.complex, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i


