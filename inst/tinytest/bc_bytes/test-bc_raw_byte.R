
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen

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

types <- "raw"

# pmin ====
basefun <- function(x, y) {
  out <- pmin(as.integer(x), as.integer(y)) |> as.raw()
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- function(x, y) bc.raw(x, y, "pmin")

res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i



# pmax ====
basefun <- function(x, y) {
  out <- pmax(as.integer(x), as.integer(y)) |> as.raw()
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- function(x, y) bc.raw(x, y, "pmax")

res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i



# diff ====
basefun <- function(x, y) {
  out <- abs(as.integer(x) - as.integer(y)) |> as.raw()
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- function(x, y) bc.raw(x, y, "diff")

res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i
