
# set-up ====
library(broadcast)
get_times <- function(obj, j) {
  nms <- names(res$expression)
  j <- which(nms == j)
  idx <- rowSums(obj$gc[[j]][, 2:3]) == 0
  times <- obj$time[[j]][idx]
  return(times)
}


# loop 2d ====
gc()
dimsizes <- seq(1250L, 9500L,  by = 750L)
print(dimsizes)
niter <- length(dimsizes)
median_bc <- q1_bc <- q3_bc <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- c(n, 1L)
  y.dims <- c(1L, n)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  
  median_bc[counter] <- median(bc_all)
  
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  
  counter <- counter + 1L
}

median_bc <- median_bc * 1000
q1_bc <- q1_bc * 1000
q3_bc <- q3_bc * 1000

save(
  dimsizes, median_bc, q1_bc, q3_bc,
  file = "benchmarks/bm_bc_2d.RData"
)



# loop 3d ====
gc()
dimsizes <- seq(65L, 450L,  by = 35L)
print(dimsizes)
niter <- length(dimsizes)
median_bc <- q1_bc <- q3_bc <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 2)[1:3]
  y.dims <- rep(c(1L, n), 2)[1:3]
  
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  
  median_bc[counter] <- median(bc_all)
  
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  
  
  
  counter <- counter + 1L
}

median_bc <- median_bc * 1000
q1_bc <- q1_bc * 1000
q3_bc <- q3_bc * 1000

save(
  dimsizes, median_bc, q1_bc, q3_bc,
  file = "benchmarks/bm_bc_3d.RData"
)


# loop 4d ====
gc()
dimsizes <- seq(9L, 99L,  by = 10L)
print(dimsizes)
niter <- length(dimsizes)
median_bc <- q1_bc <- q3_bc <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 2)[1:4]
  y.dims <- rep(c(1L, n), 2)[1:4]
  
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  
  median_bc[counter] <- median(bc_all)
  
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  
  
  
  counter <- counter + 1L
}

median_bc <- median_bc * 1000
q1_bc <- q1_bc * 1000
q3_bc <- q3_bc * 1000

save(
  dimsizes, median_bc, q1_bc, q3_bc,
  file = "benchmarks/bm_bc_4d.RData"
)




# loop 5d ====
gc()
dimsizes <- seq(6L, 39L,  by = 3L)
print(dimsizes)
niter <- length(dimsizes)
median_bc <- q1_bc <- q3_bc <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 3)[1:5]
  y.dims <- rep(c(1L, n), 3)[1:5]
  
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  
  median_bc[counter] <- median(bc_all)
  
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  
  
  
  counter <- counter + 1L
}


median_bc <- median_bc * 1000
q1_bc <- q1_bc * 1000
q3_bc <- q3_bc * 1000

save(
  dimsizes, median_bc, q1_bc, q3_bc,
  file = "benchmarks/bm_bc_5d.RData"
)



# loop 6d ====
gc()
dimsizes <- seq(3L, 21L,  by = 2L)
print(dimsizes)
niter <- length(dimsizes)
median_bc <- q1_bc <- q3_bc <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 3)[1:6]
  y.dims <- rep(c(1L, n), 3)[1:6]
  
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  
  median_bc[counter] <- median(bc_all)
  
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  
  
  
  counter <- counter + 1L
}

median_bc <- median_bc * 1000
q1_bc <- q1_bc * 1000
q3_bc <- q3_bc * 1000


save(
  dimsizes, median_bc, q1_bc, q3_bc,
  file = "benchmarks/bm_bc_6d.RData"
)


# loop 7d ====
gc()
dimsizes <- seq(2L, 14L,  by = 1L)
print(dimsizes)
niter <- length(dimsizes)
median_bc <- q1_bc <- q3_bc <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 4)[1:7]
  y.dims <- rep(c(1L, n), 4)[1:7]
  
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  
  median_bc[counter] <- median(bc_all)
  
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  
  
  
  counter <- counter + 1L
}


median_bc <- median_bc * 1000
q1_bc <- q1_bc * 1000
q3_bc <- q3_bc * 1000

save(
  dimsizes, median_bc, q1_bc, q3_bc,
  file = "benchmarks/bm_bc_7d.RData"
)
gc()

