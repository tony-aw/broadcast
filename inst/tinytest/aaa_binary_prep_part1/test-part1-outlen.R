
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
enumerate <- 0L

.rcpp_virt_make_outlen <- broadcast:::.rcpp_virt_make_outlen

# out_dim is NULL ====
for(xLen in c(1, 10, 2^31+10)) {
  for(yLen in c(1, 10, 2^31+10)) {
    expect_equal(
      .rcpp_virt_make_outlen(NULL, xLen, yLen),
      max(xLen, yLen)
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
    
  }
}

# out_dim is not NULL ====

for(iSample in 1:10) {
  xLen <- sample(c(1, 10, 2^31-1), 1L)
  yLen <- sample(c(1, 10, 2^31-1), 1L)
  for(iNdim in 1:16) {
    out.dim <- sample(1:16, iNdim)
    expect_equal(
      .rcpp_virt_make_outlen(out.dim, xLen, yLen),
      prod(out.dim)
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
    
  }
  
}

# out_dim is very large ====
n <- sqrt(2^52 - 1) |> as.integer()
xLen <- 0L
yLen <- 0L

out.dim <- c(n, n) |> as.integer()
expect_equal(
  .rcpp_virt_make_outlen(out.dim, xLen, yLen),
  prod(out.dim)
)

out.dim <- c(n, n) |> as.integer()
expect_equal(
  .rcpp_virt_make_outlen(out.dim, xLen, yLen),
  prod(out.dim)
)

enumerate <- enumerate + 2L

