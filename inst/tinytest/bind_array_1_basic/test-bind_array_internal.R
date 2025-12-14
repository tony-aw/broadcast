
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}



# .C_any_nonarray ====
.C_any_nonarray <- broadcast:::.C_any_nonarray

expect_error(
  .C_any_nonarray(1:10),
  pattern = "`x` must be a list"
)

x <- list(
  array(integer(0L)),
  array(integer(0L), c(0, 10)),
  array(1:10),
  matrix(1:20, 5, 4),
  array(1:27, c(3,3,3))
)
expect_false(
  .C_any_nonarray(x)
)

x <- list(
  array(integer(0L)),
  array(integer(0L), c(0, 10)),
  array(1:10),
  matrix(1:20, 5, 4),
  array(1:27, c(3,3,3)),
  1:10
)
expect_true(
  .C_any_nonarray(x)
)

enumerate <- enumerate + 3L


# .C_bind_which_comdims ====

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


# rcpp funs ====
.rcpp_bindhelper_anyinput_hasclass <- broadcast:::.rcpp_bindhelper_anyinput_hasclass

x <- array(1:27, c(3,3,3))
y <- array(1:27, c(3,3,3))
z <- array(1:27, c(3,3,3))
input <- list(x, y, z)

expect_false(
  .rcpp_bindhelper_anyinput_hasclass(input, "")
)
expect_false(
  .rcpp_bindhelper_anyinput_hasclass(input, "broadcaster")
)

x <- array(1:27, c(3,3,3))
y <- array(1:27, c(3,3,3))
z <- array(1:27, c(3,3,3))
bcr(x) <- TRUE
input <- list(x, y, z, x, y, z)
expect_true(
  .rcpp_bindhelper_anyinput_hasclass(input, "broadcaster")
)
input <- sample(input)
expect_true(
  .rcpp_bindhelper_anyinput_hasclass(input, "broadcaster")
)
enumerate <- enumerate + 4L
