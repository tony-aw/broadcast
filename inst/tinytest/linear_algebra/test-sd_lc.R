
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# basic tests ====
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
    sd_lc(X, vc, j)
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
  sd_lc(X, vc)
)
enumerate <- enumerate + 1L


# distributional tests ====
funlist <- list( # normal distribution already tested above, so not needed here
  \(p) rbeta(1000, p, p+1),
  \(p) rbinom(1000, 10, p/10),
  \(p) rcauchy(1000, 0, p),
  \(p) rchisq(1000, p),
  \(p) rexp(1000, p),
  \(p) rf(1000, p, p+1),
  \(p) rgamma(1000, p),
  \(p) rgeom(1000, p/10),
  \(p) rhyper(1000, 100-p, (100 - p) - p*2, ((100 - p) - p*2) - p*3),
  \(p) rlnorm(1000, 1, p),
  \(p) rpois(1000, p),
  \(p) rt(1000, p),
  \(p) runif(1000, p, p + 1),
  \(p) rweibull(1000, p)
)

make_vc <- function(x, y, z) {
  data <- data.frame(x, y, z)
  return(cov(data))
}

for(i in seq_along(funlist)) {
  for(j in seq_along(funlist)) {
    for(k in seq_along(funlist)) {
      
      p <- sample(1:10, 3)
      x <- funlist[[i]](p[1])
      y <- funlist[[i]](p[2])
      z <- funlist[[i]](p[3])
      
      mult <- sample(1:10, 3)
      dim(mult) <- c(3, 1)
      lc <- mult[1] * x + mult[2] * y + mult[3] * z
      vc <- make_vc(x, y, z)
      
      sd_real <- sd(lc)
      sd_la <- sqrt(t(mult) %*% vc %*% mult) |> as.vector()
      sd_bc <- sd_lc(t(mult), vc)
      
      expect_equal(
        round(sd_real, 6),
        round(sd_bc, 6)
      ) |> errorfun()
      expect_equal(
        round(sd_la, 6),
        round(sd_bc, 6)
      ) |> errorfun()
      enumerate <- enumerate + 2L
    }
  }
}



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
    sd_lc(i, vc),
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
    sd_lc(X, i),
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
    sd_lc(X, vc, i),
    pattern = "`bad_rp` must be a numeric scalar"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

expect_error(
  sd_lc(cbind(X, X), vc),
  pattern = "`X` and `vc` do not have correctly corresponding dimensions!"
)
enumerate <- enumerate + 1L

