
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


ortho <- broadcast:::.rcpp_bc_dbl_ov



# column vector by row vector ====

x.dim <- c(100, 1)
y.dim <- c(1, 100)
x <- array(sample(1:100), dim = x.dim)
y <- array(sample(1:100), dim = y.dim)
out.dim <- pmax(x.dim, y.dim) |> as.integer()
out.len <- prod(out.dim)

expect_true(
  broadcast:::.C_dims_all_orthogonal(x.dim, y.dim)
) |> errorfun()

expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
expected[is.nan(expected)] <- NA
out <- ortho(x, y, TRUE, out.dim, out.len, 1L)
out[is.nan(out)] <- NA
dim(out) <- out.dim
expect_equal(
  out, expected
) |> errorfun()
enumerate <- enumerate + 2L



# row vector by column vector ====

x.dim <- c(1, 100)
y.dim <- c(100, 1)
x <- array(sample(1:100), dim = x.dim)
y <- array(sample(1:100), dim = y.dim)
out.dim <- pmax(x.dim, y.dim) |> as.integer()
out.len <- prod(out.dim)

expect_true(
  broadcast:::.C_dims_all_orthogonal(x.dim, y.dim)
) |> errorfun()

expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
expected[is.nan(expected)] <- NA
out <- ortho(x, y, FALSE, out.dim, out.len, 1L)
out[is.nan(out)] <- NA
dim(out) <- out.dim
expect_equal(
  out, expected
) |> errorfun()
enumerate <- enumerate + 2L

