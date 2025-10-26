
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
types <- c("logical", "integer", "double")
gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))



# plus ====
bc.fun <- function(x, y) {
  broadcaster(x) <- broadcaster(y) <- TRUE
  out <- x + y
  return(unclass(out))
}
base.fun <- function(x, y) {
  out <- x + y
  dim(out) <- bc_dim(x, y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, "complex", types)
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun, base.fun, types, "complex")
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# min ====
bc.fun <- function(x, y) {
  broadcaster(x) <- broadcaster(y) <- TRUE
  out <- x - y
  return(unclass(out))
}
base.fun <- function(x, y) {
  out <- x - y
  dim(out) <- bc_dim(x, y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, "complex", types)
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun, base.fun, types, "complex")
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# multiply ====
bc.fun <- function(x, y) {
  broadcaster(x) <- broadcaster(y) <- TRUE
  out <- x * y
  return(unclass(out))
}
base.fun <- function(x, y) {
  out <- x * y
  out[is.na(x)|is.na(y)] <- NA
  dim(out) <- bc_dim(x, y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, "complex", types)
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun, base.fun, types, "complex")
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# div ====
bc.fun <- function(x, y) {
  broadcaster(x) <- broadcaster(y) <- TRUE
  out <- round(x / y, 6) # division in complex is sensitive to rounding errors
  return(unclass(out))
}
base.fun <- function(x, y) {
  out <- round(x / y, 6) # division in complex is sensitive to rounding errors
  out[is.na(x)|is.na(y)] <- NA
  dim(out) <- bc_dim(x, y)
  return(out)
}
# not doing complex by other, 'cause that one's REALLY weird
res <- .test_binary(bc.fun, base.fun, types, "complex")
enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


