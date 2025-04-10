# set-up ====
library(broadcast)
library(abind)

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
save(bm_abind, file = "benchmarks/bm_abind.RData")


n <- 1e4
x <- array(rnorm(10), c(n, 1))
y <- array(rnorm(10), c(1, n))
gc()
bm_outer <- bench::mark(
  Rfast = Rfast::Outer(x, y, "+"),
  broadcast = bc.num(x, y, "+"),
  min_iterations = 100,
  check = FALSE # because Rfast flips the dimensions of the results
)
summary(bm_outer)
plot(bm_outer)
save(bm_outer, file = "benchmarks/bm_outer.RData")
