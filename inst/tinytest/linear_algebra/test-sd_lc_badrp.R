
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

gen_vcov <- function(n) {
  upper <- rnorm((n^2)/2 - (n/2))
  vc <- matrix(0.0, n, n)
  vc[upper.tri(vc)] <- upper
  vc <- vc + t(vc)
  diag(vc) <- sum(abs(upper)) + abs(rnorm(n))
  return(vc)
}


coercefun <- list(
  as.double,
  as.integer,
  as.logical
)


# missing/special values tests ====

for(i in 1:50) { # multiple iterations to take into account randomness
  
  # make data:
  vc <- datasets::ability.cov$cov
  nobs <- 100
  nvars <- nrow(vc)
  n <- nobs * nvars
  vc[1,1] <- sample(c(-10, NA, NaN, Inf, -Inf), 1)
  
  
  # test real:
  X <- matrix(sample(c(1:6, NaN, NA, -Inf, Inf)), nobs, nvars)
  out <- rowSums((X %*% vc) * X) |> sqrt()
  expect_equal(
    out,
    sd_lc(X, vc)
  ) |> errorfun()
  
  
  # test integer:
  X <- matrix(sample(c(1:9, NA_integer_)), nobs, nvars)
  out <- rowSums((X %*% vc) * X) |> sqrt()
  expect_equal(
    out,
    sd_lc(X, vc)
  ) |> errorfun()
  
  
  # test logical:
  X <- matrix(sample(c(TRUE, FALSE, NA), 10, TRUE), nobs, nvars)
  out <- rowSums((X %*% vc) * X) |> sqrt()
  expect_equal(
    out,
    sd_lc(X, vc)
  ) |> errorfun()
  
  
  enumerate <- enumerate + 3L
  
}



# bad variance replacements ====
for(iBadRp in c(-1e6, 1e6, NA, NaN, Inf, -Inf)) {
  for(iCoe in seq_along(coercefun)) {
    for(i in 1:10) { # multiple iterations to take into account randomness
      
      # make data:
      vc <- datasets::ability.cov$cov
      nobs <- 100
      nvars <- nrow(vc)
      n <- nobs * nvars
      ind <- colnames(vc)[sample(seq_len(nvars), 2)]
      vc[ind, ind] <- sample(c(-10, NA, NaN, Inf, -Inf), 1)
      
      
      # perform test:
      X <- matrix(coercefun[[iCoe]](sample(1:100)), nobs, nvars)
      out <- rowSums((X %*% vc) * X)
      ind1 <- which(out < 0)
      ind2 <- which(out >= 0)
      out[ind1] <- iBadRp
      out[ind2] <- sqrt(out[ind2])
      expect_equal(
        out,
        sd_lc(X, vc, iBadRp)
      ) |> errorfun()
      
      
      enumerate <- enumerate + 1L
      
    }
  }
  
}

