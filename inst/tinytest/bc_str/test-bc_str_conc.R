
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
types <- "character"


# basic tests ====

x <- as.array(sample(letters))
y <- as.array(sample(letters))
expect_equal(
  bc.str(x, y, "+") |> as.vector(),
  as.vector(paste0(x, y))
)

x <- as.array(sample(letters))
y <- as.array(sample(letters))
expect_equal(
  bc.str(x, y, "+") |> as.vector(),
  as.vector(paste0(x, y))
)
enumerate <- enumerate + 2L



# plus ====
basefun <- function(x, y) {
  out <- ifelse(is.na(x) | is.na(y), NA_character_, paste0(x, y))
  dim(out) <- bc_dim(x, y)
  return(out)
}
bc.fun <- \(x, y) bc.str(x, y, "+")

res <- .test_binary(bc.fun, basefun, types, types)
expect_equal(
  res$expected,
  res$out
)




# attributes tests ====
bc.fun <- function(x, y) { bc.str(x, y, "+")}

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
bc.fun <- function(x, y) { bc.str(x, y, "+")}
res <- .test_binary_zerolen(bc.fun, is.character, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i




