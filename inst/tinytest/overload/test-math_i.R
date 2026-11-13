
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.return_missing <- broadcast:::.return_missing
types <- c("logical", "integer", "int53")
ab <- broadcast:::.as.broadcaster


# mod ====
nres <- 5 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
base.fun <- function(x, y) {
  return(x %% y)
}
bc.fun <- function(x, y) {
  bcr(x) <- bcr(y) <- TRUE
  return(x %% y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

# test results:
expect_equal(
  expected, out
)



# idiv ====
nres <- 5 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
base.fun <- function(x, y) {
  return(x %/% y)
}
bc.fun <- function(x, y) {
  bcr(x) <- bcr(y) <- TRUE
  return(x %/% y)
}
res <- .test_binary(bc.fun, base.fun, types, types)

# test results:
expect_equal(
  expected, out
)


