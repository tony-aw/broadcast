
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# bc.i overflow ====
expect_equal(
  bc.i(2^53, 1, "+"),
  Inf
)
expect_equal(
  bc.i(-2^53, 1, "-"),
  -Inf
)
expect_equal(
  bc.i(2^53, 2, "*"),
  Inf
)
expect_equal(
  bc.i(-2^53, 0.5, "%/%"),
  -Inf
)
expect_equal(
  bc.i(2^53, 2, "^"),
  Inf
)

enumerate <- enumerate + 5L


