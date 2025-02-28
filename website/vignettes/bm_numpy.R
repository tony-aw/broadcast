# set-up ====
library(broadcast)
library(tinycodet)
import_as(~rt, "reticulate")
np <- rt$import("numpy", convert = FALSE)
gc <- rt$import("gc", convert = FALSE)
get_times <- function(obj, j) {
  nms <- names(res$expression)
  j <- which(nms == j)
  idx <- rowSums(obj$gc[[j]][, 2:3]) == 0
  times <- obj$time[[j]][idx]
  return(times)
}
gc$disable()

# loop
median_bc <- median_np <- q1_bc <- q1_np <- q3_bc <- q3_np <- vector("numeric", 8)
counter <- 1L
target_len <- 9e6

for(i in 2:9) {
  print(i)
  n <- round(target_len^(1/i)) |> as.integer()
  len <- n^i
  cat("i = ", i, "\n")
  cat("n = ", n, "\n")
  cat("len = ", len, "\n")
  x.dims <- rep(c(n, 1L), i - 1)[1:i]
  y.dims <- rep(c(1L, n), i - 1)[1:i]
  a.dims <- rt$r_to_py(as.list(x.dims))
  b.dims <- rt$r_to_py(as.list(y.dims))
  
  npa <- np$random$random_sample(a.dims)
  npb <- np$random$random_sample(b.dims)
  a <- array(runif(100), x.dims)
  b <- array(runif(100), y.dims)
  
  res <- bench::mark(
    broadcast = bc.num(a, b, "+"),
    `numpy (NO conversion to R)` = npa + npb,
    check = FALSE,
    min_iterations = 100
  )
  bc_all <- get_times(res, "broadcast")
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
  median_bc, median_np, q1_bc, q3_bc, q1_np, q3_np,
  file = "bm_numpy_loop.RData"
)


# BigSmall ====
n <- 200L
npa <- np$random$rand(n, n, 1L, n)
npb <- np$random$rand(n, n, 1L, 1L)

a <- array(rnorm(100), c(n, n, 1L, n))
b <- array(rnorm(100), c(n, n, 1, 1))

bm_numpy_bs <- bench::mark(
  broadcast = bc.num(a, b, "+"),
  `numpy (no conversion to R)` = npa + npb,
  check = FALSE,
  min_iterations = 100,
)
summary(bm_numpy_bs)
ggplot2::autoplot(bm_numpy_bs)
save(bm_numpy_bs, file = "bm_numpy_bs.RData")



# Orthogonal Vectors ====
n <- 2000L
npa <- np$random$rand(n, 1L)
npb <- np$random$rand(1L, n)
a <- array(rnorm(n), c(n, 1))
b <- array(rnorm(n), c(1, n))

bm_numpy_ortho <- bench::mark(
  broadcast = bc.num(a, b, "+"),
  `numpy (no conversion to R)` = npa + npb,
  check = FALSE,
  min_iterations = 100,
)
summary(bm_numpy_ortho)
ggplot2::autoplot(bm_numpy_ortho)
save(bm_numpy_ortho, file = "bm_numpy_ortho.RData")



# mergeable ====

n <- 60L
npa <- np$random$rand(n, 1L, 1L, n)
npb <- np$random$rand(n, n, n, 1L)

a <- array(rnorm(100), c(n, 1L, 1L, n))
b <- array(rnorm(100), c(n, n, n, 1L))

bm_numpy_mer <- bench::mark(
  broadcast = bc.num(a, b, "+"),
  `numpy (no conversion to R)` = npa + npb,
  check = FALSE,
  min_iterations = 200,
)
summary(bm_numpy_mer)
ggplot2::autoplot(bm_numpy_mer)
save(bm_numpy_mer, file = "bm_numpy_mer.RData")




# irregular array (3d) ====

n <- 200L

npa <- np$random$rand(n, 1L, n)
npb <- np$random$rand(n, n, 1L)

a <- array(rnorm(100), c(n, 1, n))
b <- array(rnorm(100), c(n, n, 1))


bm_numpy_irr <- bench::mark(
  broadcast = bc.num(a, b, "+"),
  `numpy (no conversion to R)` = npa + npb,
  check = FALSE,
  min_iterations = 100,
)
summary(bm_numpy_irr)
ggplot2::autoplot(bm_numpy_irr)
save(bm_numpy_irr, file = "bm_numpy_irr.RData")



# large arrays ====

n <- 26L
npa <- np$random$rand(n, 1L, n, 1L, n)
npb <- np$random$rand(n, n, 1L, n, 1L)

a.dim <- c(n, rep(c(1L, n), 2))
b.dim <- c(n, rep(c(n, 1L), 2))
a <- array(rnorm(100), a.dim)
b <- array(rnorm(100), b.dim)

bm_numpy_large <- bench::mark(
  broadcast = bc.num(a, b, "+"),
  `numpy (no conversion to R)` = npa + npb,
  check = FALSE,
  min_iterations = 200,
)
summary(bm_numpy_large)
ggplot2::autoplot(bm_numpy_large)
save(bm_numpy_large, file = "bm_numpy_large.RData")

