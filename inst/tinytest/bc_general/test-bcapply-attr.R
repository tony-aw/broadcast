# test binary attributes
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen


op <- \(x, y) length(x) == length(y)
bc.fun <- \(x, y) bcapply(x, y, op)
types <- c("raw", "logical", "integer", "double", "complex", "character", "list")
res <- .test_binary_class(bc.fun, types, types)
expect_equal(
  res$expected_bc,
  res$out_bc
)
expect_equal(
  res$expected_comm,
  res$out_comm
)

enumerate <- enumerate + res$i

res <- .test_binary_zerolen(bc.fun, is.list, types, types)
expect_equal(
  res$expected_bc,
  res$out_bc
)
expect_equal(
  res$expected_comm,
  res$out_comm
)

enumerate <- enumerate + res$i