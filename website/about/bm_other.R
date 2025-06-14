# set-up ====
library(broadcast)
library(abind)
library(collapse)


# bind ====
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


# outer====
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
save(bm_outer, file = "benchmarks/bm_outer.RData")


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
save(bm_collapse_row, file = "benchmarks/bm_collapse_row.RData")

# 
# n <- 8e3
# x <- matrix(rnorm(10), n, n)
# v <- array(rnorm(10), c(n, 1))
# V <- drop(v) # need to make `v` a regular vector for collapse %c+%
# bm_collapse_col <- bench::mark(
#   collapse = x %c+% V,
#   broadcast = bc.d(x, v, "+"),
#   min_iterations = 100
# )
# summary(bm_collapse_col)
# plot(bm_collapse_col)
# save(bm_collapse_col, file = "benchmarks/bm_collapse_col.RData")
