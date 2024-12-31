
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


make_dims_starts <- function(n) {
  lapply(1:n, \(i) rep(c(sample(2:5, 1L), 1L))) |> unlist()
}
make_dims_ends <- function(n) {
  lapply(1:n, \(i) rep(c(1L, sample(2:5, 1L)))) |> unlist()
}

ortho <- broadcast:::.rcpp_bc_dbl_o

# xstarts ====

for(i in sample(1:7, 4)) { # dimension 2 to 14 (i.e, 2*1 to 2*7)
  
  x.dim <- make_dims_starts(i)
  y.dim <- make_dims_ends(i)
  x <- array(sample(1:100), dim = x.dim)
  y <- array(sample(1:100), dim = y.dim)
  x.dcp <- c(1, cumprod(x.dim))
  y.dcp <- c(1, cumprod(y.dim))
  out.dim <- pmax(x.dim, y.dim) |> as.integer()
  out.len <- prod(out.dim)
  
  expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
  out <- ortho(x, y, x.dcp, y.dcp, out.dim, out.len, TRUE, 1L)
  dim(out) <- out.dim
  expect_equal(
    out, expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}



# ystarts ====

for(i in sample(1:7, 4)) { # dimensions2 to 14 (i.e, 2*1 to 2*7)
  
  x.dim <- make_dims_ends(i)
  y.dim <- make_dims_starts(i)
  x <- array(sample(1:100), dim = x.dim)
  y <- array(sample(1:100), dim = y.dim)
  x.dcp <- c(1, cumprod(x.dim))
  y.dcp <- c(1, cumprod(y.dim))
  out.dim <- pmax(x.dim, y.dim) |> as.integer()
  out.len <- prod(out.dim)
  
  expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
  out <- ortho(x, y, x.dcp, y.dcp, out.dim, out.len, FALSE, 1L)
  dim(out) <- out.dim
  expect_equal(
    out, expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


