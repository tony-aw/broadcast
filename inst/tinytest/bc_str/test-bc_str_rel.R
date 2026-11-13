
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen


types <- "character"
prec <- sqrt(.Machine$double.eps)


# basic tests ====

x <- as.array(sample(letters))
y <- as.array(sample(letters))
expect_equal(
  bc.str(x, y, "==") |> as.vector(),
  as.vector(x == y)
)
expect_equal(
  bc.str(x, y, "!=") |> as.vector(),
  as.vector(x != y)
)

x <- as.array(sample(letters))
y <- as.array(sample(letters))
expect_equal(
  bc.str(x, y, "==") |> as.vector(),
  as.vector(x == y)
)
expect_equal(
  bc.str(x, y, "!=") |> as.vector(),
  as.vector(x != y)
)

enumerate <- enumerate + 4L



# equals ====
basefun <- function(x, y) {
  out <- x == y
  return(out)
}
bc.fun <- \(x, y) bc.str(x, y, "==")
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
bc.fun <- \(x, y) bc.str(x, y, "!=")
res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)
enumerate <- enumerate + res$i





# attributes tests ====
bc.fun <- function(x, y) { bc.str(x, y, "==")}

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
bc.fun <- function(x, y) { bc.str(x, y, "==")}
res <- .test_binary_zerolen(bc.fun, is.logical, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
enumerate <- enumerate + res$i


