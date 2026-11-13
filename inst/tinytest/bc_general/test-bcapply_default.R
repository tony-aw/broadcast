
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary

.return_missing <- broadcast:::.return_missing
gen <- function(n) sample(list(letters, month.abb, 1:10), n, TRUE)


# recursive ====

i <- 1L

op <- function(x, y) {
  c(length(x) == length(y), typeof(x) == typeof(y))
}
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bcapply(x, y, op)

res <- .test_binary(bc.fun, basefun, "list", "list", correctNaN = FALSE)
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# errors ====
x <- 1:10
y <- array(1:10, c(1, 10))
expect_error(
  bcapply(x, y, ~ hello),
  pattern = "`f` must be a function",
  fixed = TRUE
)
f <- \(x) x^2
expect_error(
  bcapply(x, y, f),
  pattern = "`f` must be a function that takes in exactly 2 arguments",
  fixed = TRUE
)
expect_error(
  bcapply(x, y, \(x, y)paste0(x, y), v = letters),
  pattern = "`v` must be a single string or `NULL`"
)
expect_error(
  bcapply(x, y, \(x, y)paste0(x, y), v = "a"),
  pattern = "unsupported type specified for `v`"
)
enumerate <- enumerate + 4L

