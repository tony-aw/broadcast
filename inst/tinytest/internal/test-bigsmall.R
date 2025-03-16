
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


make_dims <- function(n) {
  sample(c(1L, 5L), n, TRUE)
}

bs <- broadcast:::.rcpp_bc_dec_bs

# bigx ====

for(i in seq(2, 8, 2)) {
  
  x.dim <- rep(5L, i)
  y.dim <- make_dims(i)
  x <- array(sample(1:100), dim = x.dim)
  y <- array(sample(1:100), dim = y.dim)
  x.dcp <- c(1, cumprod(x.dim))
  y.dcp <- c(1, cumprod(y.dim))
  by_x <- rep(1L, i)
  by_y <- ifelse(y.dim == 1L, 0L, 1L)
  out.dim <- pmax(x.dim, y.dim) |> as.integer()
  out.len <- prod(out.dim)
  
  expected <- x + rep_dim(y, out.dim)
  expected[is.nan(expected)] <- NA
  out <- bs(x, y, by_x, by_y, x.dcp, y.dcp, out.dim, out.len, TRUE, 1L)
  out[is.nan(out)] <- NA
  dim(out) <- out.dim
  expect_equal(
    out, expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}



# bigy ====

for(i in seq(2, 8, 2)) { # dimension 2 to 14 (i.e, 2*1 to 2*7)
  
  x.dim <- make_dims(i)
  y.dim <- rep(5L, i)
  x <- array(sample(1:100), dim = x.dim)
  y <- array(sample(1:100), dim = y.dim)
  x.dcp <- c(1, cumprod(x.dim))
  y.dcp <- c(1, cumprod(y.dim))
  by_x <- ifelse(x.dim == 1L, 0L, 1L)
  by_y <- rep(1L, i)
  out.dim <- pmax(x.dim, y.dim) |> as.integer()
  out.len <- prod(out.dim)
  
  expected <- rep_dim(x, out.dim) + y
  expected[is.nan(expected)] <- NA
  out <- bs(x, y, by_x, by_y, x.dcp, y.dcp, out.dim, out.len, FALSE, 1L)
  out[is.nan(out)] <- NA
  dim(out) <- out.dim
  expect_equal(
    out, expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

