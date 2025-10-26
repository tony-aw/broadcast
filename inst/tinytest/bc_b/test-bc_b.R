
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
types <- c("logical", "integer", "raw")


# and ====
bc.fun <- function(x, y) bc.b(x, y, "&")
base.fun <- function(x, y) {
  out <- as_bool(x) & as_bool(y)
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





# or ====
bc.fun <- function(x, y) bc.b(x, y, "|")
base.fun <- function(x, y) {
  out <- as_bool(x) | as_bool(y)
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




# xor ====
bc.fun <- function(x, y) bc.b(x, y, "xor")
base.fun <- function(x, y) {
  out <- as_bool(x) != as_bool(y)
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




# nand ====
bc.fun <- function(x, y) bc.b(x, y, "nand")
base.fun <- function(x, y) {
  out <- !as_bool(x) & !as_bool(y)
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





# equals ====
bc.fun <- function(x, y) bc.b(x, y, "==")
base.fun <- function(x, y) {
  out <- as_bool(x) == as_bool(y)
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
  out <- as_bool(x) != as_bool(y)
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
  out <- as_bool(x) < as_bool(y)
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
  out <- as_bool(x) > as_bool(y)
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
  out <- as_bool(x) <= as_bool(y)
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
  out <- as_bool(x) >= as_bool(y)
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

