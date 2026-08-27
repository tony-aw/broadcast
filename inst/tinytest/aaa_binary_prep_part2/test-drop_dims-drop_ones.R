

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_virt_drop_dims <- broadcast:::.rcpp_virt_drop_dims
.rcpp_clone <- broadcast:::.rcpp_clone

basefun <- function(x.dim, y.dim) {
  x.dim <- broadcast:::.rcpp_clone(x.dim)
  y.dim <- broadcast:::.rcpp_clone(y.dim)
  ndim <- length(x.dim)
  if(length(x.dim) > 1L && length(y.dim) > 1L) {
    ind <- x.dim != 1L | y.dim != 1L ## equivalent to !(x.dim == 1L && y.dim == 1L)
    if(any(ind)) {
      x.dim <- x.dim[ind]
      y.dim <- y.dim[ind]
    }
    else {
      x.dim <- NULL
      y.dim <- NULL
    }
  }
  
  x.ndim <- length(x.dim)
  y.ndim <- length(y.dim)
  x.dim <- c(x.dim, rep(1L, ndim - x.ndim))
  y.dim <- c(y.dim, rep(1L, ndim - y.ndim))
  
  out <- list(x.dim, y.dim, x.ndim, y.ndim)
  return(out)
  
}


# remove ones ====
for(iNDim in 2:16) {
  x.dim <- sample(c(1L, 2L), iNDim, TRUE)
  y.dim <- sample(c(1L, 2L), iNDim, TRUE)
  x.ndim <- length(x.dim)
  y.ndim <- length(y.dim)
  x.len <- prod(x.dim)
  y.len <- prod(y.dim)
  
  if(x.len != 1L && y.len != 1L) {
    
    out <- basefun(x.dim, y.dim)
    .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
    
    expect_equal(
      out[[1L]],
      x.dim
    ) |> errorfun()
    
    expect_equal(
      out[[2L]],
      y.dim
    ) |> errorfun()
    
    expect_equal(
      out[[3L]],
      x.ndim
    ) |> errorfun()
    
    expect_equal(
      out[[4L]],
      y.ndim
    ) |> errorfun()
    
    enumerate <- enumerate + 4L
    
  }
  
}


# no ones ====
for(iNDim in 2:16) {
  x.dim <- sample(2:3, iNDim, TRUE)
  y.dim <- sample(2:3, iNDim, TRUE)
  x.ndim <- length(x.dim)
  y.ndim <- length(y.dim)
  x.len <- prod(x.dim)
  y.len <- prod(y.dim)
  
  x.dim2 <- .rcpp_clone(x.dim)
  y.dim2 <- .rcpp_clone(y.dim)
  x.ndim2 <- .rcpp_clone(x.ndim)
  y.ndim2 <- .rcpp_clone(y.ndim)
  x.len2 <- .rcpp_clone(x.len)
  y.len2 <- .rcpp_clone(y.len)
  
  .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
  
  expect_equal(
    c(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len),
    c(x.dim2, y.dim2, x.ndim2, y.ndim2, x.len2, y.len2)
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
  
}

