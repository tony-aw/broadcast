
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


.C_bind_which_comdims <- broadcast:::.C_bind_which_comdims

x.dims <- sample(1:10, 5)
out.dims <- c(1, x.dims)
expect_equal(
  .C_bind_which_comdims(out.dims, 2, x.dims),
  1:5
)

x.dims <- sample(1:10, 5)
out.dims <- c(x.dims, 1)
expect_equal(
  .C_bind_which_comdims(out.dims, 1, x.dims),
  1:5
)

x.dims <- 1:5
out.dims <- 6:10
expect_equal(
  .C_bind_which_comdims(out.dims, 1, x.dims),
  integer(0L)
)

samp <- sample(1:5)
x.dims <- c(10, samp, 10)
out.dims <- c(1, samp, 1)
expect_equal(
  .C_bind_which_comdims(out.dims, 1, x.dims),
  2:6
)

enumerate <- enumerate + 4L

