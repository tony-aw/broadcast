
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

make_sample <- function(n) {
  sample(c(0:99, NA, NaN), n, TRUE)
}

basefun_mat <- function(y, sim) {
  out <- numeric(nrow(sim))
  for(i in 1:nrow(sim)) {
    out[i] <- ecdf(sim[i,])(y[i])
  }
  return(out)
}


# simple tests ====
x <- make_sample(1e6L)
y <- make_sample(1L)
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
  y <- coercefun[[i]](1)
  sim <- matrix(make_sample(100*500), 100, 500) |> coercefun[[i]]()
  expect_equal(
    round(rowMeans(bc.rel(sim, y,  "<=")), 9),
    round(ecumprob(y, sim), 9)
  ) |> errorfun()
  expect_equal(
    round(rowMeans(sim <= y), 9),
    round(ecumprob(y, as.data.frame(sim)), 9)
  ) |> errorfun()
  
  y <- make_sample(100) |> coercefun[[i]]()
  sim <- matrix(make_sample(1*500), 1, 500) |> coercefun[[i]]()
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
  sim <- matrix(make_sample(100) |> as.double(), 100, 5000)
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




