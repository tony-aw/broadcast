
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# test nsim/ncol ====
for(i in 1:10) {
  for(j in 1:10) {
    
    mat <- matrix(rnorm(i * j), i, j)
    v <- mat[1, ] |> drop()
    df <- as.data.frame(mat)
    
    expect_equal(
      broadcast:::.ecp_nsim(mat),
      j
    ) |> errorfun()
    expect_equal(
      broadcast:::.ecp_nsim(v),
      j
    ) |> errorfun()
    expect_equal(
      broadcast:::.ecp_nsim(df),
      j
    ) |> errorfun()
    
    
    enumerate <- enumerate + 3L
    
  }
}


# test nobs/nrow ====
for(i in 1:10) {
  for(j in 1:10) {
    
    mat <- matrix(rnorm(i * 5000), i, 5000)
    v <- mat[1, ] |> drop()
    df <- as.data.frame(mat)
    
    y <- rnorm(j)
    
    expect_equal(
      broadcast:::.ecp_nobs(y, mat),
      max(i, j)
    ) |> errorfun()
    expect_equal(
      broadcast:::.ecp_nobs(y, v),
      max(1, j) # 1 instead of i, because when sim is a vector, it is taken as a matrix with 1 row and n columns
    ) |> errorfun()
    expect_equal(
      broadcast:::.ecp_nobs(y, df),
      max(i, j)
    ) |> errorfun()
    
    enumerate <- enumerate + 3L
    
  }
}

