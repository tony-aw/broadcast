

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


.rcpp_max_ndim <- broadcast:::.rcpp_max_ndim

for(i in 0:16) {
  for(j in 0:16) {
    
    expected <- max(i, j)
    expected <- ifelse(expected > 4L, 16L, 4L)
    expect_equal(
      .rcpp_max_ndim(i, j),
      expected
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
    
  }
}


expect_error(
  .rcpp_max_ndim(17L, 15L),
  pattern = "no more than 16 dimensions allowed"
)
expect_error(
  .rcpp_max_ndim(15L, 17L),
  pattern = "no more than 16 dimensions allowed"
)
enumerate <- enumerate + 2L
