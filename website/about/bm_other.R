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
save(bm_base, file = "benchmarks/bm_base.RData")




# base linear algebra ====
# making objects that one might get from a usual large regression model:
n <- 3e3
X <- matrix(rnorm(n, 10), 10, n)
X1 <- X[1, , drop = FALSE]
w <- t(X1)
vc <- cov(as.data.frame(X))



gc()
bm_la <- bench::mark(
  t(w) %*% vc %*% w,
  sd_lc(X1, vc),
  check = FALSE,
  min_iterations = 100
)
summary(bm_la)
plot(bm_la)


n <- 500
X <- matrix(rnorm(n), 1e3, n)
vc <- cov(as.data.frame(X))



gc()
bm_la <- bench::mark(
  sqrt(pmax(rowSums((X %*% vc) * X), 0)),
  check = FALSE,
  min_iterations = 100
)
summary(bm_la)


gc()
bm_la <- bench::mark(
  sd_lc(X, vc),
  min_iterations = 100
)
summary(bm_la)



