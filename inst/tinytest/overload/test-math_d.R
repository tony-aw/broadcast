
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
types <- c("logical", "integer", "double")


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
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# minus ====
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
res <- .test_binary(bc.fun, base.fun, types, types)

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
  dim(out) <- bc_dim(x, y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# div ====
bc.fun <- function(x, y) {
  broadcaster(x) <- broadcaster(y) <- TRUE
  out <- x / y
  return(unclass(out))
}
base.fun <- function(x, y) {
  out <- x / y
  dim(out) <- bc_dim(x, y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)

# pow ====
bc.fun <- function(x, y) {
  broadcaster(x) <- broadcaster(y) <- TRUE
  out <- x^y
  return(unclass(out))
}
base.fun <- function(x, y) {
  out <- x^y
  dim(out) <- bc_dim(x, y)
  return(out)
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)

