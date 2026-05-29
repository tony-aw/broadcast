
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
types <- c("logical", "integer", "raw")


# equals ====
bc.fun <- function(x, y) bc.b(x, y, "==")
base.fun <- function(x, y) {
  x2 <- as_bool(x)
  y2 <- as_bool(y)
  out <- (x2 & y2) | (!x2 & !y2)
  if(is.raw(x) && is.raw(y)) {
    out <- as_raw(out)
  }
  return(out)
  
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# unequals ====
bc.fun <- function(x, y) bc.b(x, y, "!=")
base.fun <- function(x, y) {
  x2 <- as_bool(x)
  y2 <- as_bool(y)
  
  out <-  xor(x2, y2)
  if(is.raw(x) && is.raw(y)) {
    out <- as_raw(out)
  }
  return(out)
  
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# smaller ====
bc.fun <- function(x, y) bc.b(x, y, "<")
base.fun <- function(x, y) {
  x2 <- as_bool(x)
  y2 <- as_bool(y)
  
  out <- ifelse(is.na(x2) | is.na(y2), NA, (!x2 & y2))
  if(is.raw(x) && is.raw(y)) {
    out <- as_raw(out)
  }
  return(out)
  
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# greater ====
bc.fun <- function(x, y) bc.b(x, y, ">")
base.fun <- function(x, y) {
  x2 <- as_bool(x)
  y2 <- as_bool(y)
  
  out <- ifelse(is.na(x2) | is.na(y2), NA, (x2 & !y2))
  if(is.raw(x) && is.raw(y)) {
    out <- as_raw(out)
  }
  return(out)
  
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)



# se ====
bc.fun <- function(x, y) bc.b(x, y, "<=")
base.fun <- function(x, y) {
  x2 <- as_bool(x)
  y2 <- as_bool(y)
  
  out <- ifelse(is.na(x2) | is.na(y2), NA, (!x2 & y2) | (y2 == x2))
  if(is.raw(x) && is.raw(y)) {
    out <- as_raw(out)
  }
  return(out)
  
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


# ge ====
bc.fun <- function(x, y) bc.b(x, y, ">=")
base.fun <- function(x, y) {
  
  x2 <- as_bool(x)
  y2 <- as_bool(y)
  
  out <- (ifelse(is.na(x2) | is.na(y2), NA, x2 & !y2) | (y2 == x2))
  if(is.raw(x) && is.raw(y)) {
    out <- as_raw(out)
  }
  return(out)
  
}
res <- .test_binary(bc.fun, base.fun, types, types)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)


