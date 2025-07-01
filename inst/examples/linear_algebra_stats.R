
vc <- datasets::ability.cov$cov
X <- matrix(rnorm(1000), 1000, ncol(vc))

solve(vc)
cinv(vc) # faster than `solve()`, but only works on positive definite matrices
all(round(solve(vc), 6) == round(cinv(vc), 6)) # they're the same

sd_lc(X, vc)

     
