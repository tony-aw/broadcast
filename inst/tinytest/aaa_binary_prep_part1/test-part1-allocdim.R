
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_virt_alloc_dim <- broadcast:::.rcpp_virt_alloc_dim

# error ====
for(i in 1:4) {
  x <- 1:i
  expect_error(
    .rcpp_virt_alloc_dim(x, 0L),
    pattern = "bad input given"
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}

for(i in 5:16) {
  x <- 1:i
  expect_error(
    .rcpp_virt_alloc_dim(x, 4L),
    pattern = "bad input given"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

for(i in 17:20) {
  x <- 1:i
  expect_error(
    .rcpp_virt_alloc_dim(x, 16L),
    pattern = "bad input given"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


# NULL dims ====
expect_equal(
  .rcpp_virt_alloc_dim(NULL, 4L),
  rep(1L, 4L)
)

expect_equal(
  .rcpp_virt_alloc_dim(NULL, 16L),
  rep(1L, 16L)
)


# regular dims ====
for(i in 0:4) {
  x <- sample(1:10, i)
  expect_equal(
    .rcpp_virt_alloc_dim(x, 4L),
    c(x, rep(1L, 4L - length(x)))
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}

for(i in 5:16) {
  x <- sample(1:16, i)
  expect_equal(
    .rcpp_virt_alloc_dim(x, 16L),
    c(x, rep(1L, 16L - length(x)))
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}

