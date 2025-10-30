# set-up ====
library(collapse)
library(abind)
library(broadcast)



# bind a few large arrays====
n <- 110L
nms <- function(n) sample(letters, n, TRUE)
x <- array(as.double(1:25), c(n, n, n))
y <- array(as.double(-1:-25), c(n, n, n))
dimnames(x) <- lapply(dim(x), nms)
dimnames(y) <- lapply(dim(y), nms)
input <- list(x, y, x)

gc()
bm_abind <- bench::mark(
  abind = abind::abind(input, along = 2),
  broadcast = bind_array(input, 2),
  min_iterations = 100,
  check = FALSE # because abind adds empty dimnames
)
summary(bm_abind)
ggplot2::autoplot(bm_abind)
save(bm_abind, file = "bm_abind_arrays.RData")


# bind many vectors ====
input <- rep(list(array(1:1000, c(1, 1000))), 100)
gc()
bm_abind <- bench::mark(
  abind = abind::abind(input, along = 2),
  broadcast = bind_array(input, 2),
  min_iterations = 100,
  check = FALSE # because abind adds empty dimnames
)
summary(bm_abind)
ggplot2::autoplot(bm_abind)
save(bm_abind, file = "bm_abind_vectors.RData")




# Rfast::outer====
n <- 9e3
x <- array(rnorm(10), c(1, n))
y <- array(rnorm(10), c(n, 1))

gc()
bm_outer <- bench::mark(
  Rfast = Rfast::Outer(x, y, "+"),
  broadcast = bc.d(x, y, "+"),
  min_iterations = 100
)
summary(bm_outer)
plot(bm_outer)
save(bm_outer, file = "bm_outer.RData")


# collapse ops ====
n <- 8e3
x <- matrix(rnorm(10), n, n)
v <- array(rnorm(10), c(1, n))

gc()
bm_collapse_row <- bench::mark(
  collapse = x %r+% v,
  broadcast = bc.d(x, v, "+"),
  min_iterations = 100
)
summary(bm_collapse_row)
plot(bm_collapse_row)
save(bm_collapse_row, file = "bm_collapse_row.RData")



# base replication ====
n <- 450
x <- array(rnorm(10), c(1, n, 1))
y <- array(rnorm(10), c(n, 1, n))

gc()
bm_base <- bench::mark(
  base = x[rep(1, n), , rep(1, n)] + y[, rep(1, n), ],
  broadcast = bc.d(x, y, "+"),
  min_iterations = 100
)
summary(bm_base)
plot(bm_base)
save(bm_base, file = "bm_base.RData")



# base sweep ====
n <- 2000
x <- matrix(rnorm(n), n, n)
cm <- array(colMeans(x), c(1, n))


gc()
bm_sweep <- bench::mark(
  sweep = sweep(x, 2, cm, "=="),
  broadcast = bc.d(x, cm, "=="),
  min_iterations = 100
)
summary(bm_sweep)
plot(bm_sweep)
save(bm_sweep, file = "bm_sweep.RData")


# base outer ====
n <- 2000
x <- array(rnorm(10), c(1, n))
y <- array(rnorm(10), c(n, 1))
xv <- as.vector(x)
yv <- as.vector(y)

gc()
bm_base_outer <- bench::mark(
  outer = outer(xv, yv, "=="),
  broadcast = bc.d(x, y, "=="),
  min_iterations = 100
)
summary(bm_base_outer)
plot(bm_base_outer)
save(bm_base_outer, file = "bm_base_outer.RData")



# cinv ====
n <- 500
upper <- rnorm((n^2)/2 - (n/2))
vc <- matrix(0.0, n, n)
vc[upper.tri(vc)] <- upper
vc <- vc + t(vc)
diag(vc) <- sum(abs(upper)) + abs(rnorm(n))


bm_cinv <- bench::mark(
  solve = solve(vc),
  cinv = cinv(vc),
  min_iterations = 100
)
summary(bm_cinv)
plot(bm_cinv)
save(bm_cinv, file = "bm_cinv.RData")


# nested vs dimensional list ====
x <- lapply(1:200, function(x) sample(1:200))
x <- rep(list(x), 200)
x2 <- cast_hier2dim(x)
bm_hier_vs_dim <- bench::mark(
  for(i in 1:length(x)) x[[i]][[1L]],
  x2[1L, , drop = FALSE],
  min_iterations = 200,
  check = FALSE
)
summary(bm_hier_vs_dim)
plot(bm_hier_vs_dim)
save(bm_hier_vs_dim, file = "bm_hier_vs_dim.RData")

