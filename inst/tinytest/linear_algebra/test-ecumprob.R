
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

nsim <- 500L

make_sim1 <- function(p, rdistr, nsim) {
  type <- typeof(rdistr(1L, p[1L]))
  out <- vector(type, length(p) * nsim)
  dim(out) <- c(length(p), nsim)
  for(i in seq_along(p)) {
    out[i, ] <- rdistr(nsim, p[i])
  }
  return(out)
}


make_sim2 <- function(p1, p2, rdistr, nsim) {
  type <- typeof(rdistr(1L, p1[1L], p2[[1L]]))
  out <- vector(type, length(p1) * nsim)
  dim(out) <- c(length(p1), nsim)
  for(i in seq_along(p1)) {
    out[i, ] <- rdistr(nsim, p1[i], p2[i])
  }
  return(out)
}

basefun_mat <- function(y, sim) {
  out <- numeric(nrow(sim))
  for(i in 1:nrow(sim)) {
    out[i] <- ecdf(sim[i,])(y[i])
  }
  return(out)
}


# simple tests ====
x <- rnorm(1e6L)
y <- rnorm(1L)
expect_equal(
  round(ecdf(x)(y), 9),
  round(ecumprob(y, x), 9)
)
expect_equal(
  round(mean(x <= y), 9),
  round(ecumprob(y, x), 9)
)
enumerate <- enumerate + 1L


# broadcasting & type-setting tests ====
coercefun <- list(
  as_dbl,
  as_int,
  as_bool
)
for(i in seq_along(coercefun)) {
  
  # broadcast y:
  y <- coercefun[[i]](1)
  sim <- matrix(rnorm(100*500), 100, 500) |> coercefun[[i]]()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, sim), 9)
  ) |> errorfun()
  expect_equal(
    round(rowMeans(sim <= y), 9),
    round(ecumprob(y, as.data.frame(sim)), 9)
  ) |> errorfun()
  
  
  # broadcast sim:
  y <- sample(0:99) |> coercefun[[i]]()
  sim <- matrix(rnorm(1*500), 1, 500) |> coercefun[[i]]()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, sim), 9)
  ) |> errorfun()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, as.data.frame(sim)), 9)
  ) |> errorfun()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, as.vector(sim)), 9)
  ) |> errorfun()
  
  
  enumerate <- enumerate + 5L
  
}


# coerce to highest type tests ====
coercefun <- list(
  as_dbl,
  as_int
)
for(i in seq_along(coercefun)) {
  sim <- matrix(sample(0:99) |> as.double(), 100, 5000)
  y <- 1.0
  sim2 <- coercefun[[i]](sim)
  y2 <- coercefun[[i]](y)
  
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y2, sim), 9)
  ) |> errorfun()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, sim2), 9)
  ) |> errorfun()
  
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y2, as.data.frame(sim)), 9)
  ) |> errorfun()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, as.data.frame(sim2)), 9)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
}



# 1-param distributional tests ====
n <- 10L
rdistr <- list( # normal distribution already tested above, so not needed here
  \(nsim, p) rbinom(nsim, 1, p/n) |> as.logical(), # Bernoulli
  \(nsim, p) rexp(nsim, p),
  \(nsim, p) rgeom(nsim, p/n),
  \(nsim, p) rpois(nsim, p) |> as.integer(),
  \(nsim, p) rt(nsim, p),
  \(nsim, p) runif(nsim, p, p + 1),
  \(nsim, p) rweibull(nsim, p),
  \(nsim, p) rf(nsim, p, p+1),
  \(nsim, p) rhyper(nsim, 100-p, (100 - p) - p*2, ((100 - p) - p*2) - p*3)
)

p <- sample(1:n, 1000L, replace = TRUE)

for(i in seq_along(rdistr)) {
  sim <- make_sim1(p, rdistr[[i]], nsim)
  y <- rdistr[[i]](nrow(sim), p)
  expected_stats <- round(basefun_mat(y, sim), 9L)
  expected_la <- rowMeans(bc.rel(sim, y, "<="))
  out1 <- round(ecumprob(y, sim), 9L)
  out2 <- out1 <- round(ecumprob(y, as.data.frame(sim)), 9L)
  
  expect_equal(
    expected_stats,
    out1
  ) |> errorfun()
  expect_equal(
    expected_stats,
    out2
  ) |> errorfun()
  
  expect_equal(
    expected_la,
    out1
  ) |> errorfun()
  expect_equal(
    expected_la,
    out2
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
}



# 2-param distributional tests ====

rdistr <- list(
  \(nsim, mu, disp) rnorm(nsim, mu, disp),
  \(nsim, shape1, shape2) rbeta(nsim, shape1, shape2),
  \(nsim, n, p) rbinom(nsim, n, p/10) |> as.integer(),
  \(nsim, loc, scale) rcauchy(nsim, loc, scale),
  \(nsim, mu, disp) {
    return(rgamma(nsim, shape = mu^2 / disp, rate = mu / disp * mu))
  },
  \(nsim, mu, disp) rlnorm(nsim, mu, disp),
  \(nsim, mu, size) rnbinom(nsim, mu = mu, size = size) |> as.integer(),
  \(nsim, a, b) runif(nsim, a, a + b),
  \(nsim, shape, scale) rweibull(nsim, shape, scale)
)

mu <- sample(1:10, 1000L, replace = TRUE)
disp <- sample(1:10, 1000L, replace = TRUE)

for(i in seq_along(rdistr)) {
  sim <- make_sim2(mu, disp, rdistr[[i]], nsim)
  y <- rdistr[[i]](nrow(sim), mu, disp)
  expected_stats <- round(basefun_mat(y, sim), 8L)
  expected_la <- rowMeans(bc.rel(sim, y, "<="))
  
  out1 <- round(ecumprob(y, sim), 9L)
  out2 <- out1 <- round(ecumprob(y, as.data.frame(sim)), 9L)
  
  expect_equal(
    expected_stats,
    out1
  ) |> errorfun()
  expect_equal(
    expected_stats,
    out2
  ) |> errorfun()
  
  expect_equal(
    expected_la,
    out1
  ) |> errorfun()
  expect_equal(
    expected_la,
    out2
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
}


# zero-length ====
expect_equal(
  ecumprob(numeric(0L), rnorm(100)),
  numeric(0L)
)
expect_equal(
  ecumprob(rnorm(100), matrix(numeric(0L))),
  numeric(0L)
)
expect_equal(
  ecumprob(rnorm(100), matrix(NA, 0L, 100) |> as.data.frame()),
  numeric(0L)
)
enumerate <- enumerate + 3L


# errors - general ====
y <- rnorm(100)
sim <- matrix(rnorm(100 * 1000), 100, 1000)
expect_error(
  ecumprob(y, array(sim, c(100, 100, 10))),
  pattern = "`sim` must be a matrix (or data.frame)",
  fixed = TRUE
)
expect_error(
  ecumprob(as.logical(y), sim),
  pattern = "`y` and `sim` must be of the same type"
)
expect_error(
  ecumprob(as.raw(y), as.raw(sim)),
  pattern = "`y` must be numeric or logical"
)
expect_error(
  ecumprob(array(y), sim),
  pattern = "`y` must be a vector"
)
expect_error(
  ecumprob(y, sim, ~ 0.1),
  pattern = "`eps` must be a numeric scalar"
)
expect_error(
  ecumprob(y, sim, seq(0, 0.9, by = 0.1)),
  pattern = "`eps` must be a numeric scalar"
)
expect_error(
  ecumprob(y, sim, 1L),
  pattern = "`eps` cannot be smaller than 0 or larger than 0.1"
)
enumerate <- enumerate + 7L


# errors - sim is matrix ====
y <- rnorm(100)
sim <- matrix(rnorm(100 * 1000), 100, 1000)
expect_error(
  ecumprob(y, sim[, 1:10]),
  pattern = "at least 500 columns of simulated values must be provided"
)
expect_error(
  ecumprob(y, sim[1:2, ]),
  pattern = "not comformable for broadcasting",
  fixed = TRUE
)

enumerate <- enumerate + 2L


# errors - sim is vector ====
expect_message(
  ecumprob(y[1L], sim[1:500L]),
  pattern = "`sim` is given as a dimensionless vector, and will be treated as a matrix with 1 row and `length(sim)` columns",
  fixed = TRUE
)
expect_error(
  ecumprob(y[1L], sim[1:300L]),
  pattern = "at least 500 columns of simulated values must be provided"
)
enumerate <- enumerate + 2L



# errors - sim is data.frame ====
expect_error(
  ecumprob(y, as.data.frame(sim[, 1:10])),
  pattern = "at least 500 columns of simulated values must be provided"
)
expect_error(
  ecumprob(y, as.data.frame(sim[1:2, ])),
  pattern = "not comformable for broadcasting",
  fixed = TRUE
)
expect_error(
  ecumprob(y, matrix(as.raw(rnorm(100)), 1, 500) |> as.data.frame()),
  pattern = "the columns of `sim` must be numeric or logical"
)
sim2 <- cbind(
  data.frame(rnorm(100)),
  matrix(as.logical(rnorm(100)), 1, 500) |> as.data.frame()
)
expect_error(
  ecumprob(y, sim2),
  pattern = "all columns of `sim` must be of the same type"
)

enumerate <- enumerate + 4L



# errors - sim is a 1d array ====
y <- rnorm(100)
sim <- matrix(rnorm(1000), 1000)
expect_error(
  ecumprob(y, sim),
  pattern = "at least 500 columns of simulated values must be provided"
)

enumerate <- enumerate + 1L

