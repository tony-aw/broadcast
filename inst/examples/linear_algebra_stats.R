
# variances ====
vc <- datasets::ability.cov$cov
X <- matrix(rnorm(100), 100, ncol(vc))

solve(vc)
cinv(vc) # faster than `solve()`, but only works on positive definite matrices
all(round(solve(vc), 6) == round(cinv(vc), 6)) # they're the same

sd_lc(X, vc)



# ecumprob() ====

sim <- rnbinom(10 * 1e4, mu = 3, size = 2) |> matrix(10, 1e4)
y <- sample(0:9)

# vector:
pnbinom(y[1], mu = 3, size = 2) # real probability
ecumprob(y[1], sim[1, , drop = TRUE]) # approximation

# matrix:
cbind(
  real = pnbinom(y, mu = 3, size = 2), # real probability
  approx = ecumprob(y, sim) # approximation
)

# data.frame:
cbind(
  real = pnbinom(y, mu = 3, size = 2), # real probability
  approx = ecumprob(y, as.data.frame(sim)) # approximation
)


