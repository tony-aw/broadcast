
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

mergeable <- broadcast:::.rcpp_is_mergeable_with_prev
mergedims <- broadcast:::.rcpp_mergedims


# check full orthogonals are never merged ====

for(i in 1:8) {
  pow <- 1/(i * 2)
  n <- ceiling(1e6^pow) |> as.integer()
  x.dim <- rep(c(1L, n), i)
  y.dim <- rep(c(n, 1L), i)
  
  m <- mergeable(x.dim == 1L, y.dim == 1L)
  expect_equal(
    list(x.dim, y.dim),
    mergedims(x.dim, y.dim, m)
  ) |> errorfun()
  
  x.dim <- rep(c(1L, n), i)
  y.dim <- rep(c(n, 1L), i)
  
  m <- mergeable(y.dim == 1L, x.dim == 1L)
  expect_equal(
    list(y.dim, x.dim),
    mergedims(y.dim, x.dim, m)
  ) |> errorfun()
  
  
}

enumerate <- enumerate + 16


# check int overflow safety ====
n <- as.integer(2^31 - 2)
x.dim <- c(n, n, 1L)
y.dim <- c(1L, 1L, n)
m <- mergeable(x.dim == 1L, y.dim == 1L)
expect_equal(
  list(x.dim, y.dim),
  mergedims(x.dim, y.dim, m)
) |> errorfun()

x.dim <- c(n, 1L, 1L)
y.dim <- c(1L, n, n)
m <- mergeable(x.dim == 1L, y.dim == 1L)
expect_equal(
  list(x.dim, y.dim),
  mergedims(x.dim, y.dim, m)
) |> errorfun()

enumerate <- enumerate + 2L


# check regular functionality ====
n <- 10
x <- c(n, n, 1, n, n, n, n) |> as.integer()
y <- c(1, 1, n, 1, 1, 1, 1) |> as.integer()
expected <- list(
  c(n^2, 1, n^4) |> as.integer(),
  c(1, n, 1) |> as.integer()
)
m <- mergeable(x==1L, y==1L)
out <- mergedims(x, y, m)
expect_equal(
  expected, out
)

enumerate <- enumerate + 1L
