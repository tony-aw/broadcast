
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.return_missing <- broadcast:::.return_missing
ab <- broadcast:::.as.broadcaster

types <- c("logical", "integer", "double", "raw")


# smaller ====
base.fun1 <- function(x, y) {
  out <- x < y
  return(ab(out))
}
base.fun2 <- function(x, y) {
  return(x < y)
}
bc.fun1 <- function(x, y) {
  ab(x) < ab(y)
}
bc.fun2 <- function(x, y) {
  bc.rel(x, y, "<")
}
res <- .test_binary(bc.fun1, base.fun1, types, types)
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun2, base.fun2, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# greater ====
base.fun1 <- function(x, y) {
  out <- x > y
  return(ab(out))
}
base.fun2 <- function(x, y) {
  return(x > y)
}
bc.fun1 <- function(x, y) {
  ab(x) > ab(y)
}
bc.fun2 <- function(x, y) {
  bc.rel(x, y, ">")
}
res <- .test_binary(bc.fun1, base.fun1, types, types)
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun2, base.fun2, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# leq ====
base.fun1 <- function(x, y) {
  out <- x <= y
  return(ab(out))
}
base.fun2 <- function(x, y) {
  return(x <= y)
}
bc.fun1 <- function(x, y) {
  ab(x) <= ab(y)
}
bc.fun2 <- function(x, y) {
  bc.rel(x, y, "<=")
}
res <- .test_binary(bc.fun1, base.fun1, types, types)
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun2, base.fun2, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i



# geq ====
base.fun1 <- function(x, y) {
  out <- x >= y
  return(ab(out))
}
base.fun2 <- function(x, y) {
  return(x >= y)
}
bc.fun1 <- function(x, y) {
  ab(x) >= ab(y)
}
bc.fun2 <- function(x, y) {
  bc.rel(x, y, ">=")
}
res <- .test_binary(bc.fun1, base.fun1, types, types)
expect_equal(
  res$expected, res$out
)
res <- .test_binary(bc.fun2, base.fun2, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


