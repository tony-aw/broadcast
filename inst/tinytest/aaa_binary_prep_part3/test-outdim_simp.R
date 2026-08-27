
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L

.rcpp_virt_make_outdim_simp <- broadcast:::.rcpp_virt_make_outdim_simp

# return NULL ===
x.dim <- rep(1L, 4L)
y.dim <- rep(1L, 4L)
expect_null(.rcpp_virt_make_outdim_simp(x.dim, y.dim, 0L))
x.dim <- sample(1:4)
y.dim <- sample(1:4)
expect_null(.rcpp_virt_make_outdim_simp(x.dim, y.dim, 0L))
enumerate <- enumerate + 2L

# return integer vector ====
for(iNdim in 1:4) {
  x.dim <- sample(1:iNdim)
  y.dim <- sample(1:iNdim)
  
  expected <- c(pmax(x.dim, y.dim), rep(1L, 4L - iNdim))
  
  expect_equal(
    .rcpp_virt_make_outdim_simp(x.dim, y.dim, iNdim),
    expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

for(iNdim in 5:16) {
  x.dim <- sample(1:3, iNdim, TRUE)
  y.dim <- sample(1:3, iNdim, TRUE)
  
  expected <- c(pmax(x.dim, y.dim), rep(1L, 16L - iNdim))
  
  expect_equal(
    .rcpp_virt_make_outdim_simp(x.dim, y.dim, iNdim),
    expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}





