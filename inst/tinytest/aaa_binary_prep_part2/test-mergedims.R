
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

mergedims <- function(x.dim, y.dim) {
  ndim <- broadcast:::.rcpp_max_ndim(length(x.dim), length(y.dim))
  x.dim2 <- broadcast:::.rcpp_virt_alloc_dim(x.dim, ndim)
  y.dim2 <- broadcast:::.rcpp_virt_alloc_dim(y.dim, ndim)
  x.ndim <- ndim(x.dim)
  y.ndim <- ndim(y.dim)
  
  broadcast:::.rcpp_mergedims_set(x.dim2, y.dim2, x.ndim, y.ndim)
  out <- list(
    x.dim2[seq_len(x.ndim)],
    y.dim2[seq_len(y.ndim)]
  )
  return(out)
}


# check full orthogonals are never merged ====

for(i in 1:8) {
  pow <- 1/(i * 2)
  n <- ceiling(1e6^pow) |> as.integer()
  x.dim <- rep(c(1L, n), i)
  y.dim <- rep(c(n, 1L), i)
  
  expect_equal(
    list(x.dim, y.dim),
    mergedims(x.dim, y.dim)
  ) |> errorfun()
  
  x.dim <- rep(c(1L, n), i)
  y.dim <- rep(c(n, 1L), i)
  
  expect_equal(
    list(y.dim, x.dim),
    mergedims(y.dim, x.dim)
  ) |> errorfun()
  
  
}

enumerate <- enumerate + 16


# check int overflow safety ====
n <- as.integer(2^31 /2)
x.dim <- c(n, n, 1L)
y.dim <- c(1L, 1L, n)
expect_equal(
  list(x.dim, y.dim),
  mergedims(x.dim, y.dim)
)

x.dim <- c(n, 1L, 1L)
y.dim <- c(1L, n, n)
expect_equal(
  list(x.dim, y.dim),
  mergedims(x.dim, y.dim)
)

enumerate <- enumerate + 2L


# check regular functionality ====
n <- 10
x <- c(n, n, 1, n, n, n, n) |> as.integer()
y <- c(1, 1, n, 1, 1, 1, 1) |> as.integer()
expected <- list(
  c(n^2, 1, n^4) |> as.integer(),
  c(1, n, 1) |> as.integer()
)
out <- mergedims(x, y)
expect_equal(
  expected, out
)

enumerate <- enumerate + 1L

