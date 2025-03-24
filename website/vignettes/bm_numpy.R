
# set-up ====
library(broadcast)
library(reticulate)
np <- import("numpy", convert = FALSE)
gc <- import("gc", convert = FALSE)
gc$disable()
get_times <- function(obj, j) {
  nms <- names(res$expression)
  j <- which(nms == j)
  idx <- rowSums(obj$gc[[j]][, 2:3]) == 0
  times <- obj$time[[j]][idx]
  return(times)
}


# loop 2d ====
gc()
dimsizes <- seq(1000L, 3000L,  by = 200L)
niter <- length(dimsizes)
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- c(n, 1L)
  y.dims <- c(1L, n)
  a.dims <- r_to_py(as.list(x.dims))
  b.dims <- r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  np_all <- get_times(res, "numpy (NO conversion to R)")
  median_bc[counter] <- median(bc_all)
  median_np[counter] <- median(np_all)
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  q1_np[counter] <- quantile(np_all, 0.25)
  q3_np[counter] <- quantile(np_all, 0.75)
  
  counter <- counter + 1L
}




save(
  dimsizes, median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop_2d.RData"
)



# loop 3d ====
gc()
dimsizes <- seq(20L, 150L,  by = 10L)
niter <- length(dimsizes)
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 2)[1:3]
  y.dims <- rep(c(1L, n), 2)[1:3]
  a.dims <- r_to_py(as.list(x.dims))
  b.dims <- r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  np_all <- get_times(res, "numpy (NO conversion to R)")
  median_bc[counter] <- median(bc_all)
  median_np[counter] <- median(np_all)
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  q1_np[counter] <- quantile(np_all, 0.25)
  q3_np[counter] <- quantile(np_all, 0.75)
  
  counter <- counter + 1L
}




save(
  dimsizes, median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop_3d.RData"
)


# loop 4d ====
gc()
dimsizes <- seq(10L, 55L,  by = 5L)
niter <- length(dimsizes)
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 2)[1:4]
  y.dims <- rep(c(1L, n), 2)[1:4]
  a.dims <- r_to_py(as.list(x.dims))
  b.dims <- r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  np_all <- get_times(res, "numpy (NO conversion to R)")
  median_bc[counter] <- median(bc_all)
  median_np[counter] <- median(np_all)
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  q1_np[counter] <- quantile(np_all, 0.25)
  q3_np[counter] <- quantile(np_all, 0.75)
  
  counter <- counter + 1L
}




save(
  dimsizes, median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop_4d.RData"
)




# loop 5d ====
gc()
dimsizes <- seq(5L, 25L,  by = 5L)
niter <- length(dimsizes)
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 3)[1:5]
  y.dims <- rep(c(1L, n), 3)[1:5]
  a.dims <- r_to_py(as.list(x.dims))
  b.dims <- r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  np_all <- get_times(res, "numpy (NO conversion to R)")
  median_bc[counter] <- median(bc_all)
  median_np[counter] <- median(np_all)
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  q1_np[counter] <- quantile(np_all, 0.25)
  q3_np[counter] <- quantile(np_all, 0.75)
  
  counter <- counter + 1L
}




save(
  dimsizes, median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop_5d.RData"
)



# loop 6d ====
gc()
dimsizes <- seq(1L, 15L,  by = 2L)
niter <- length(dimsizes)
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 3)[1:6]
  y.dims <- rep(c(1L, n), 3)[1:6]
  a.dims <- r_to_py(as.list(x.dims))
  b.dims <- r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  np_all <- get_times(res, "numpy (NO conversion to R)")
  median_bc[counter] <- median(bc_all)
  median_np[counter] <- median(np_all)
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  q1_np[counter] <- quantile(np_all, 0.25)
  q3_np[counter] <- quantile(np_all, 0.75)
  
  counter <- counter + 1L
}




save(
  dimsizes, median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop_6d.RData"
)


# loop 7d ====
gc()
dimsizes <- seq(1L, 10L,  by = 1L)
niter <- length(dimsizes)
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", niter)
counter <- 1L

for(i in seq_along(dimsizes)) {
  print(i)
  n <- dimsizes[i]
  x.dims <- rep(c(n, 1L), 4)[1:7]
  y.dims <- rep(c(1L, n), 4)[1:7]
  a.dims <- r_to_py(as.list(x.dims))
  b.dims <- r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 200
  )
  bc_all <- get_times(res, "broadcast")
  if(length(bc_all) < 100) {
    stop("too few benchmarks for 'R'")
  }
  np_all <- get_times(res, "numpy (NO conversion to R)")
  median_bc[counter] <- median(bc_all)
  median_np[counter] <- median(np_all)
  q1_bc[counter] <- quantile(bc_all, 0.25)
  q3_bc[counter] <- quantile(bc_all, 0.75)
  q1_np[counter] <- quantile(np_all, 0.25)
  q3_np[counter] <- quantile(np_all, 0.75)
  
  counter <- counter + 1L
}




save(
  dimsizes, median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop_7d.RData"
)
