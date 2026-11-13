
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.return_missing <- broadcast:::.return_missing
.test_binary <- broadcast:::.test_binary
ab <- broadcast:::.as.broadcaster
types <- "raw"


# bit-wise and ====
op <- "&"
base.fun <- function(x, y) {
  return(ab(x & y))
}
bc.fun <- function(x, y) {
  return(ab(x) & ab(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# bit-wise and ====
op <- "|"
base.fun <- function(x, y) {
  return(ab(x | y))
}
bc.fun <- function(x, y) {
  return(ab(x) | ab(y))
}
res <- .test_binary(bc.fun, base.fun, types, types)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i

