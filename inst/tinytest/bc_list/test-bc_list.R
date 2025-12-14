
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 3:
  out <- lapply(1:n, \(n)sample(c(1, 3), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}
.return_missing <- broadcast:::.return_missing
gen <- function(n) sample(list(letters, month.abb, 1:10, NULL), n, TRUE)

types <- "list"

op <- function(x, y) {
  c(length(x) == length(y), typeof(x) == typeof(y))
}
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  dim(out) <- bc_dim(x, y)
  return(out)
}

bc.fun <- \(x, y) { bc.list(x, y, op)}

res <- .test_binary(bc.fun, basefun, types, types, correctNaN = FALSE)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i





# attributes tests ====
res <- .test_binary_class(bc.fun, types, types)
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
expect_equal(
  res$expected_ma, res$out_ma
)
enumerate <- enumerate + res$i


# zerolen tests ====
res <- .test_binary_zerolen(bc.fun, is.list, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i





