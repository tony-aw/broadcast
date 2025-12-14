
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- list(
  as.raw(0:9),
  sample(c(TRUE, FALSE, NA), 10, TRUE),
  1:10,
  rnorm(10),
  rnorm(10) + rnorm(10) * -1i,
  ~ foo1 + foo2
)

# arrangement = 0 ====

expected <- lapply(x, as.character)
expected <- do.call(c, expected)
out <- cast_shallow2atomic(x)
expect_equal(
  out, expected
)

enumerate <- enumerate + 1L

# arrangement = 1 ====
tempfun <- function(x) {
  length(x) <- 10L
  return(x)
}
expected <- lapply(x, as.character)
expected <- lapply(expected, tempfun)
out <- cast_shallow2atomic(x, arrangement = 1)
expect_equal(
  out,
  simplify2array(expected)
)
enumerate <- enumerate + 1L


# arrangement = -1 ====
tempfun <- function(x) {
  length(x) <- 10L
  return(x)
}
expected <- lapply(x, as.character)
expected <- lapply(expected, tempfun)
out <- cast_shallow2atomic(x, arrangement = -1)
expect_equal(
  out,
  t(simplify2array(expected))
)
enumerate <- enumerate + 1L
