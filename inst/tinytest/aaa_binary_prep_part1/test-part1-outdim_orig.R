
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L

.rcpp_virt_make_outdim_orig <- broadcast:::.rcpp_virt_make_outdim_orig

# both sides NULL ====
x <- 1:10
y <- 1:10

expect_null(
  .rcpp_virt_make_outdim_orig(x, y, dim(x), dim(y), ndim(x), ndim(y))
)
enumerate <- enumerate + 1L


# one side is NULL ====
# NOTE:
# we are here testing .rcpp_virt_make_outdim_orig on its own;
# this function does NOT conformalize the dimensions
# thus we don't test orthogonal vectors and such
x <- 1:10
y <- array(1:10)
expect_equal(
  .rcpp_virt_make_outdim_orig(x, y, dim(x), dim(y), ndim(x), ndim(y)),
  dim(y)
)

x <- array(1:10)
y <- 1:10
expect_equal(
  .rcpp_virt_make_outdim_orig(x, y, dim(x), dim(y), ndim(x), ndim(y)),
  dim(x)
)
enumerate <- enumerate + 2L



# full orthogonal arrays ====

for(i in 1:10) {
  x.dim <- rep(c(10L, 1L), 16L)[1:i]
  y.dim <- rep(c(1L, 10L), 16L)[1:i]
  x <- array(1:10, x.dim)
  y <- array(1:10, y.dim)
  
  expect_equal(
    .rcpp_virt_make_outdim_orig(x, y, dim(x), dim(y), ndim(x), ndim(y)),
    pmax(x.dim, y.dim)
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
  
}



# long dimension ====

x <- array(as.raw(1:10), 2^31 - 1)
y <- array(as.raw(10:1), 2^31 - 1)

expect_equal(
  .rcpp_virt_make_outdim_orig(x, y, dim(x), dim(y), ndim(x), ndim(y)),
  as.integer(2^31 - 1)
)


# VERY large arrays ====

for(i in 1:16) {
  
  print(i)
  
  n <- floor((2^52 - 1)^(1/i))
  n <- ifelse(n >= (2^31 - 1), 2^31 - 1, n)
  n <- ifelse(n^i >= (2^52 - 1), floor(sqrt(n)), n)
  print(n)
  
  x.dim <- rep(c(n, 1L), 16L)[1:i] |> as.integer()
  y.dim <- rep(c(1L, n), 16L)[1:i] |> as.integer()
  fakex <- 1:10
  fakey <- 1:10
  
  expect_equal(
    .rcpp_virt_make_outdim_orig(fakex, fakey, x.dim, y.dim, i, i),
    pmax(x.dim, y.dim)
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
  
}


# error ====

x <- array(as.raw(1:10), c(2^31 - 1, 1))
y <- array(as.raw(10:1), c(1, 2^31 - 1))
expect_error(
  .rcpp_virt_make_outdim_orig(x, y, dim(x), dim(y), ndim(x), ndim(y)),
  pattern = "broadcasting will exceed maximum size"
)
enumerate <- enumerate + 1L


# No error ====
expect_silent(
  .rcpp_virt_make_outdim_orig(x, x, dim(x), dim(x), ndim(x), ndim(x))
)
enumerate <- enumerate + 1L
