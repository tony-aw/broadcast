# set-up ====
library(broadcast)
library(tinycodet)

# Orthogonal Vectors ====
n <- 2500L
a <- array(rnorm(n), c(n, 1))
b <- array(rnorm(n), c(1, n))

bm_outer_ortho <- bench::mark(
  bc = bc.num(a, b, "+"),
  outer = outer(a, b, "+"),
  check = FALSE,
  min_iterations = 200,
)
summary(bm_outer_ortho)
ggplot2::autoplot(bm_outer_ortho)
save(bm_outer_ortho, file = "bm_outer_ortho.RData")



# mergeable ====

n <- 60L

a <- array(rnorm(100), c(n, 1L, 1L, n))
b <- array(rnorm(100), c(n, n, n, 1L))

bm_outer_mer <- bench::mark(
  bc = bc.num(a, b, "+"),
  outer = outer(a, b, "+"),
  check = FALSE,
  min_iterations = 200,
)
summary(bm_outer_mer)
ggplot2::autoplot(bm_outer_mer)
save(bm_outer_mer, file = "bm_outer_mer.RData")




# irregular array (3d) ====

n <- 200L
a <- array(rnorm(100), c(n, 1, n))
b <- array(rnorm(100), c(n, n, 1))


bm_outer_irr <- bench::mark(
  bc = bc.num(a, b, "+"),
  outer = outer(a, b, "+"),
  check = FALSE,
  min_iterations = 100,
)
summary(bm_outer_irr)
ggplot2::autoplot(bm_outer_irr)
save(bm_outer_irr, file = "bm_outer_irr.RData")



# large arrays ====

n <- 25L
a.dim <- c(n, rep(c(1L, n), 2))
b.dim <- c(n, rep(c(n, 1L), 2))
a <- array(rnorm(100), a.dim)
b <- array(rnorm(100), b.dim)

bm_outer_large <- bench::mark(
  bc = bc.num(a, b, "+"), # bc is massively better
  outer = outer(a, b, "+"),
  check = FALSE,
  min_iterations = 200,
)
summary(bm_outer_large)
ggplot2::autoplot(bm_outer_large)
save(bm_outer_large, file = "bm_outer_large.RData")

