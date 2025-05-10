
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 3:
  out <- lapply(1:n, \(n)sample(c(1, 3), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}

for(iSample in 1:10) {
  for(iDims in 2:10) {
    for(iBc in 0:(iDims - 1)) {
      along <- iDims + 1L
      x.dim <- y.dim <- sample(2:10, iDims, TRUE)
      x.dim[sample(1:length(x.dim), iBc)] <- 1
      y.dim[sample(1:length(y.dim), iBc)] <- 1
      
      x <- array(rnorm(10), x.dim)
      y <- array(rnorm(10), y.dim)
      
      out.dim <- pmax(dim(x), dim(y))
      
      maxbc <- max(
        sum(dim(x) != out.dim),
        sum(dim(y) != out.dim)
      )
      
      if(maxbc > 0) {
        p <- sprintf(
          "maximum number of dimensions to be broadcasted (%d) exceeds `ndim2bc` (%d)",
          maxbc, 0L
        )
        expect_error(
          bind_array(list(x, y), along, ndim2bc = 0),
          pattern = p,
          fixed = TRUE
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
      }
      
      if(maxbc > 1) {
        p <- sprintf(
          "maximum number of dimensions to be broadcasted (%d) exceeds `ndim2bc` (%d)",
          maxbc, 1L
        )
        expect_error(
          bind_array(list(x, y), along),
          pattern = p,
          fixed = TRUE
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
      }
      
      if(maxbc > 2) {
        p <- sprintf(
          "maximum number of dimensions to be broadcasted (%d) exceeds `ndim2bc` (%d)",
          maxbc, 2L
        )
        expect_error(
          bind_array(list(x, y), along, ndim2bc = 2),
          pattern = p,
          fixed = TRUE
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
      }
    }
  }
}

