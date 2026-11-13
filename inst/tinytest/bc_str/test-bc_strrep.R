
# setup ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
.return_missing <- broadcast:::.return_missing
.test_binary <- broadcast:::.test_binary

bc.fun <- function(x, y) {
  bc_strrep(x, abs(y))
}
base.fun <- function(x, y) {
  strrep(x, abs(y))
}


# main tests ====

res <- suppressWarnings(
  .test_binary(bc.fun, base.fun, "character", c("integer", "int53"))
)

enumerate <- enumerate + res$i # count number of tests
# test results:
expect_equal(
  res$expected, res$out
)




# attributes tests ====
res <- suppressWarnings(
  .test_binary_class(bc_strrep, "character", c("integer", "int53"))
)
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
res <- suppressWarnings(
  .test_binary_zerolen(bc_strrep, is.character, "character", c("integer", "int53"))
)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_equal(
  res$expected_comm, res$out_comm
)
enumerate <- enumerate + res$i





