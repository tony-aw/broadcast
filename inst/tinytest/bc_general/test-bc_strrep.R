
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 5:
  out <- lapply(1:n, \(n)sample(c(1, 5), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}
.return_missing <- broadcast:::.return_missing
.test_binary <- broadcast:::.test_binary


bc.fun <- function(x, y) {
  bc_strrep(x, abs(y))
}
base.fun <- function(x, y) {
  strrep(x, abs(y))
}

res <- .test_binary(bc.fun, base.fun, "character", c("integer", "int53"))

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)
