
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

`%&%` <- bitwAnd
`%|%` <- bitwOr
`%xor%` <- bitwXor
`%nand%` <- function(x, y) bitwNot(bitwAnd(x, y))
`%==%` <- function(x, y) bitwNot(bitwXor(x, y))


# bitwise & ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here

op <- "&"

i <- 1L

basefun <- function(x, y) {
  out <- bitwAnd(x, y)
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "&")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i



# bitwise | ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here

op <- "|"

i <- 1L

basefun <- function(x, y) {
  out <- bitwOr(x, y)
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "|")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# bitwise xor ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here

op <- "xor"

i <- 1L

basefun <- function(x, y) {
  out <- bitwXor(x, y)
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "xor")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# bitwise nand ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here

op <- "nand"

i <- 1L

basefun <- function(x, y) {
  out <- bitwNot(bitwAnd(x, y))
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, op)
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i



# bitwise nor ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here

op <- "nor"

i <- 1L

basefun <- function(x, y) {
  out <- bitwNot(bitwOr(x, y))
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, op)
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# sanity tests ====
for(i in c(TRUE, FALSE)) {
  for(j in c(TRUE, FALSE)) {
    expect_equal(
      i == j,
      !xor(i, j)
    ) |> errorfun()
    
    expect_equal(
      i != j,
      xor(i, j)
    ) |> errorfun()
    
    expect_equal(
      i < j,
      !i & j
    ) |> errorfun()
    
    expect_equal(
      i > j,
      i & !j
    ) |> errorfun()
    
    expect_equal(
      i <= j,
      !i | j
    ) |> errorfun()
    
    expect_equal(
      i >= j,
      i | !j
    ) |> errorfun()
    
  }
}
enumerate <- enumerate + 6L


# bitwise equal ====
op <- "=="

i <- 1L

basefun <- function(x, y) {
  out <- x %==% y
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "==")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# bitwise unequal ====


op <- "!="

i <- 1L

basefun <- function(x, y) {
  out <- x %xor% y
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "!=")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# bitwise smaller ====


op <- "<"

i <- 1L

basefun <- function(x, y) {
  out <- bitwNot(x) %&% y
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "<")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i


# bitwise greater ====


op <- ">"

i <- 1L

basefun <- function(x, y) {
  out <- x %&% bitwNot(y)
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, ">")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i

# bitwise se ====


op <- "<="

i <- 1L

basefun <- function(x, y) {
  out <- bitwNot(x) %|% y
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, "<=")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i



# bitwise ge ====


op <- ">="

i <- 1L

basefun <- function(x, y) {
  out <- x %|% bitwNot(y)
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.bit(x, y, ">=")
res <- .test_binary(bc.fun, basefun, "integer", "integer")
expect_equal(
  res$expected, res$out
)
enumerate <- enumerate + res$i




# attributes tests (and/or) ====
bc.fun <- function(x, y) { bc.bit(x, y, "&")}

types <- "integer"

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


# zerolen tests (and/or) ====
bc.fun <- function(x, y) { bc.bit(x, y, "&")}

res <- .test_binary_zerolen(bc.fun, is.integer, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
enumerate <- enumerate + res$i



# attributes tests (rel) ====
bc.fun <- function(x, y) { bc.bit(x, y, "==")}

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


# zerolen tests (rel) ====
bc.fun <- function(x, y) { bc.bit(x, y, "==")}

res <- .test_binary_zerolen(bc.fun, is.integer, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
enumerate <- enumerate + res$i


