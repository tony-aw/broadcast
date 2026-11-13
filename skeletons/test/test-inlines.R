
library(tinytest)
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


for(iSample in 1:5) {
  for(iN in 2:16) {
    x.dim <- sample(c(1L, 2:5), iN, TRUE)
    x.dim2 <- c(x.dim, rep(1L, 16L - length(x.dim)))
    by_x <- as.integer(x.dim2 > 1L)
    dcp_x <- cumprod(c(1L, x.dim2))[1:16]
    expect_equal(
      by_x * dcp_x,
      inline_alloc_strides(x.dim)
    ) |> errorfun()
    
    expect_equal(
      x.dim2,
      inline_alloc_bounds(x.dim2, length(x.dim))
    ) |> errorfun()
    
    expect_equal(
      inline_determine_loopdepth(x.dim2),
      inline_determine_loopdepth(x.dim),
    ) |> errorfun()
    
    expect_true(
      inline_determine_loopdepth(x.dim2) <= iN,
    ) |> errorfun()
    
    bounds <- inline_alloc_bounds(x.dim2, length(x.dim))
    expect_equal(
      inline_determine_outer_Niters(bounds),
      prod(x.dim)/x.dim[1L]
    ) |> errorfun()
    
  }
}


