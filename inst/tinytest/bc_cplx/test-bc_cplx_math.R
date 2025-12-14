
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen

types <- "complex"
gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))


# basic tests ====

x <- as.array(gen() + gen() * -1i)
y <- as.array(gen() + gen() * -1i)
expect_equal(
  bc.cplx(x, y, "+") |> as.vector(),
  as.vector(x + y)
)

x <- as.array(gen() + gen() * -1i)
y <- as.array(gen() + gen() * -1i)
expect_equal(
  bc.cplx(x, y, "+") |> as.vector(),
  as.vector(x + y)
)
enumerate <- enumerate + 2L



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
  out[is.na(x) | is.na(y)] <- NA
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# div ====
bc.fun <- function(x, y) bc.cplx(x, y, "*")
base.fun <- function(x, y) {
  out <- as_cplx(x) * as_cplx(y)
  out[is.na(x) | is.na(y)] <- NA
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


