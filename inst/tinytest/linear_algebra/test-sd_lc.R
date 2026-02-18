
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


# basic tests ====
coercefun <- list(
  as.double,
  as.integer,
  as.logical
)

for(k in seq_along(coercefun)) {
  
  nobs <- 20
  nvars <- 10
  n <- nobs * nvars
  X <- matrix(coercefun[[k]](rnorm(100)), nobs, nvars)
  vc <- gen_vcov(nvars)
  
  out <- rowSums((X %*% vc) * X) |> sqrt()
  expect_equal(
    out,
    sd_lc(X, vc)
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}


vc <- datasets::ability.cov$cov
nobs <- 100
nvars <- nrow(vc)
n <- nobs * nvars

X <- matrix(rnorm(100), nobs, nvars)


out <- rowSums((X %*% vc) * X) |> sqrt()
expect_equal(
  out,
  sd_lc(X, vc)
)
enumerate <- enumerate + 1L



# test on combinations of distributions ====
funlist <- list( # normal distribution already tested above, so not needed here
  \(p) rbeta(1000, p, p+1),
  \(p) rbinom(1000, 1, p/10) |> as.logical(), # Bernoulli
  \(p) rbinom(1000, 10, p/10) |> as.integer(),
  \(p) rcauchy(1000, 0, p),
  \(p) rchisq(1000, p),
  \(p) rexp(1000, p),
  \(p) rf(1000, p, p+1),
  \(p) rgamma(1000, p),
  \(p) rgeom(1000, p/10),
  \(p) rhyper(1000, 100-p, (100 - p) - p*2, ((100 - p) - p*2) - p*3),
  \(p) rlnorm(1000, 1, p),
  \(p) rnbinom(1000, p, 1/p) |> as.integer(),
  \(p) rpois(1000, p) |> as.integer(),
  \(p) rt(1000, p),
  \(p) runif(1000, p, p + 1),
  \(p) rweibull(1000, p)
)

make_vc <- function(x, y, z, w) {
  data <- data.frame(x, y, z, w)
  return(cov(data))
}

expected_la <- expected_real <- out <- numeric(length(funlist)^3)
counter <- 1

for(i in seq_along(funlist)) {
  for(j in seq_along(funlist)) {
    for(k in seq_along(funlist)) {
      
      p <- sample(1:10, 3)
      x <- funlist[[i]](p[1])
      y <- funlist[[j]](p[2])
      z <- funlist[[k]](p[3])
      w <-( x + y * z) * rnorm(1000)
      
      mult <- sample(1:10, 4)
      dim(mult) <- c(4, 1)
      lc <- mult[1] * x + mult[2] * y + mult[3] * z + mult[4] * w
      vc <- make_vc(x, y, z, w)
      
      expected_real[counter] <- sd(lc)
      expected_la[counter] <- sqrt(t(mult) %*% vc %*% mult) |> as.vector()
      out[counter] <- sd_lc(t(mult), vc)
      counter <- counter + 1
      
    }
  }
}

expect_equal(
  round(expected_real, 6),
  round(out, 6)
)
expect_equal(
  round(expected_la, 6),
  round(out, 6)
)
enumerate <- enumerate + 2 * length(funlist)^3


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
    pattern = "`X` must be a numeric or logical matrix"
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

