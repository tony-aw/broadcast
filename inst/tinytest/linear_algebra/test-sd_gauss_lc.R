
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


for(j in c(NaN, NA, 0, -1e6, 1e6)) {
  
  nobs <- 1e3
  nvars <- 200
  n <- nobs * nvars
  X <- matrix(rnorm(1000), nobs, nvars)
  vc <- matrix(rnorm(1000), nvars, nvars)
  
  out <- numeric(nobs)
  for(i in 1:nobs) {
    a <- matrix(X[i,, drop = FALSE], ncol = 1)
    out[i] <- sqrt(t(a) %*% vc %*% a)
  }
  out[is.na(out)] <- j
  expect_equal(
    out,
    sd_gauss_lc(X, vc, j)
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}



vc <- datasets::ability.cov$cov
nobs <- 500
nvars <- nrow(vc)
n <- nobs * nvars

X <- matrix(rnorm(1000), nobs, nvars)


out <- numeric(nobs)
for(i in 1:nobs) {
  a <- matrix(X[i,, drop = FALSE], ncol = 1)
  out[i] <- sqrt(t(a) %*% vc %*% a)
}
expect_equal(
  out,
  sd_gauss_lc(X, vc)
)
enumerate <- enumerate + 1L


# errors ====
vc <- datasets::ability.cov$cov
nobs <- 500
nvars <- nrow(vc)
n <- nobs * nvars

X <- matrix(rnorm(1000), nobs, nvars)
Xlist <- list(
  as.vector(X),
  as_chr(X),
  matrix(numeric(0))
)
for(i in Xlist) {
  expect_error(
    sd_gauss_lc(i, vc),
    pattern = "`X` must be a numeric matrix"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

vcList <- list(
  as.vector(vc),
  as_chr(vc),
  as_int(vc),
  matrix(numeric(0))
)
for(i in vcList) {
  expect_error(
    sd_gauss_lc(X, i),
    pattern = "`vc` must be a variance-covariance matrix"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

badrpList <- list(
  1:10,
  "a",
  numeric(0L)
)
for(i in badrpList) {
  expect_error(
    sd_gauss_lc(X, vc, i),
    pattern = "`bad_rp` must be a numeric scalar"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

expect_error(
  sd_gauss_lc(cbind(X, X), vc),
  pattern = "`X` and `vc` do not have correctly corresponding dimensions!"
)
enumerate <- enumerate + 1L

