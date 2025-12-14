
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

gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))
types <- "complex"


# basic tests ====

x <- as.array(gen() + gen() * -1i)
y <- as.array(gen() + gen() * -1i)
expect_equal(
  bc.cplx(x, y, "==") |> as.vector(),
  as.vector(x == y)
)
expect_equal(
  bc.cplx(x, y, "!=") |> as.vector(),
  as.vector(x != y)
)

x <- as.array(gen() + gen() * -1i)
y <- as.array(gen() + gen() * -1i)
expect_equal(
  bc.cplx(x, y, "==") |> as.vector(),
  as.vector(x == y)
)
expect_equal(
  bc.cplx(x, y, "!=") |> as.vector(),
  as.vector(x != y)
)

enumerate <- enumerate + 4L



# equals ====
basefun <- function(x, y) {
  out <- x == y
  return(out)
}
bc.fun <- \(x, y) { bc.cplx(x, y, "==") }

res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i


# unequals ====
basefun <- function(x, y) {
  out <- x != y
  return(out)
}
bc.fun <- \(x, y) { bc.cplx(x, y, "!=") }

res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i



# attributes tests ====
bc.fun <- function(x, y) { bc.cplx(x, y, "==")}
types <- "complex"
res <- .test_binary_class(bc.fun, types, types)
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
expect_false(
  identical(res$expected_ma, res$out_ma)
)
enumerate <- enumerate + res$i


# zerolen tests ====
bc.fun <- function(x, y) { bc.cplx(x, y, "==")}
res <- .test_binary_zerolen(bc.fun, is.logical, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
enumerate <- enumerate + res$i


