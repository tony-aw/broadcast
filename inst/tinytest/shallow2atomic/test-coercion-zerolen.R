
# # set-up ====
enumerate <- 0L
errorfun <- function(tt) {

  if(isFALSE(tt)) stop(print(tt))
}

x <- list(
  1:11, 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, character(0L)
)
expect_true(
  is.character(cast_shallow2atomic(x))
)
expect_true(
  is.character(cast_shallow2atomic(x, 1L))
)
expect_true(
  is.character(cast_shallow2atomic(x, -1L))
)

y <- x
y[lengths(y) == 0L] <- list(NULL)
expect_true(
  length(y) == length(x)
)
expect_true(
  is.integer(cast_shallow2atomic(y))
)
expect_true(
  is.integer(cast_shallow2atomic(y, 1L))
)
expect_true(
  is.integer(cast_shallow2atomic(y, -1L))
)

enumerate <- enumerate + 6L


