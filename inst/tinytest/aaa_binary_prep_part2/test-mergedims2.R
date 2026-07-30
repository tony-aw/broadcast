
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


.rcpp_clone <- broadcast:::.rcpp_clone

# test unmergeable ====

# vector:
x.dim <- 10L
y.dim <- 10L
expected <- list(x.dim, y.dim)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)

# ortho:
x.dim <- c(100L, 1L)
y.dim <- c(1L, 90L)
expected <- list(x.dim, y.dim)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)

# sandwhich:
x.dim <- c(100L, 90L, 50L)
y.dim <- c(1L, 90L, 1L)
expected <- list(x.dim, y.dim)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)

# general:
x.dim <- c(100L, 1L, 50L, 1L)
y.dim <- c(1L, 90L, 1L, 30L)
expected <- list(x.dim, y.dim)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)

enumerate <- enumerate + 4L



# test merge all ====
x.dim <- c(10, 10, 10) |> as.integer()
y.dim <- c(10, 10, 10) |> as.integer()
expected <- list(1000L, 1000L)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test drop all ====
x.dim <- rep(1L, 5L)
y.dim <- rep(1L, 5L)
expected <- list(1L, 1L)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test drop ends, merge ins ====
# note: a current limitation of mergedims is that the first common 1L is NOT dropped
# but common 1L at the end IS dropped
x.dim <- c(1, 10, 10, 1) |> as.integer()
y.dim <-  c(1, 10, 10,  1) |> as.integer()
expected <- list(c(1L, 100L), c(1L, 100L))
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test merge ends, drop ins ====
x.dim <- c(10, 10, 1, 10, 10) |> as.integer()
y.dim <- c(10, 10, 1, 10, 10) |> as.integer()
expected <- list(10000L, 10000L)
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L



# test merge ortho ====
x.dim <- c(7, 8, 1, 9, 1, 1, 1, 1) |> as.integer()
y.dim <- c(1, 1, 1, 1, 5, 3, 4, 1) |> as.integer()
expected <- list(c(7*8*9, 1L), c(1, 5*3*4))
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L



# test merge sandwich ====
x.dim <- c(1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 1) |> as.integer()
y.dim <- c(5, 3, 4, 1, 7, 8, 9, 1, 3, 5, 4, 1) |> as.integer()
expected <- list(c(1, 7*8*9, 1), c(5*3*4, 7*8*9, 3*5*4)) 
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test merge general ====
x.dim <- c(1, 1, 1, 1, 7, 8, 9, 1, 1, 1, 1, 1) |> as.integer()
y.dim <- c(5, 3, 4, 1, 1, 1, 1, 1, 3, 5, 4, 1) |> as.integer()
expected <- list(c(1, 7*8*9, 1), c(5*3*4, 1, 3*5*4)) 
out <- mergedims(x.dim, y.dim)
expect_equal(
  expected, out
)
enumerate <- enumerate + 1L


# test self reference ====
for(i in 1:50) {
  x.dim <- sample(1:5, 8, TRUE)
  x <- array(sample(1:prod(x.dim)), x.dim)
  y <- x
  y.dim <- .rcpp_clone(dim(y))
  out <- mergedims(y.dim, y.dim)
  x.dim <- out[[1L]]
  y.dim <- out[[1L]]
  dim(y) <- y.dim
  foo <- expect_equal(
    as.vector(x),
    as.vector(y)
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

