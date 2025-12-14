
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary


# logical ====
op <- function(x, y) {
  return(x == y)
}
v <- "logical"
bc.fun1 <- \(x, y) bc.b(x, y, "==")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "logical")

res <- .test_binary(bc.fun1, bc.fun2, v, v, correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i




# integer (32bit) ====

op <- function(x, y) {
  return(x + y)
}
v <- "integer"
bc.fun1 <- \(x, y) bc.i(x, y, "+")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "integer")

res <- .test_binary(bc.fun1, bc.fun2, v, v, correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i


# double (64bit) ====

op <- function(x, y) {
  return(x + y)
}
v <- "double"
bc.fun1 <- \(x, y) bc.d(x, y, "+")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "double")

res <- .test_binary(bc.fun1, bc.fun2, v, v, correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i




# complex ====
op <- function(x, y) {
  return(x + y)
}
v <- "complex"
bc.fun1 <- \(x, y) bc.cplx(x, y, "+")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "complex")

res <- .test_binary(bc.fun1, bc.fun2, v, v, correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i



# string ====
gen <- function(n) sample(c(letters, month.abb), n, TRUE)
i <- 1L
op <- function(x, y) {
  if(is.na(x) || is.na(y)) {
    return(NA_character_)
  }
  else {
    return( paste(x, y, collapse = "", sep = ""))
  }
}
v <- "character"
bc.fun1 <- \(x, y) bc.str(x, y, "+")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "character")

res <- .test_binary(bc.fun1, bc.fun2, v, v, correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i


# raw ====
op <- function(x, y) {
  return(x & y)
}
v <- "raw"
bc.fun1 <- \(x, y) bc.bit(x, y, "&")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "raw")

res <- .test_binary(bc.fun1, bc.fun2, v, v, correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i


# mixed types case ====
op <- function(x, y) {
  return(x == y)
}
bc.fun1 <- \(x, y) bc.rel(x, y, "==")
bc.fun2 <- \(x, y) bcapply(x, y, op, v = "logical")

res <- .test_binary(bc.fun1, bc.fun2, "raw", "raw", correctNaN = FALSE)

expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i


# errors ====
x <- 1:10
y <- 1:10
expect_error(
  bcapply(x, y, \(x, y) x + y, v = "numeric"),
  pattern = "`numeric` is an ambiguous type",
  fixed = TRUE
)
enumerate <- enumerate + 1L

