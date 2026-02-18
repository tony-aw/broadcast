
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
source(file.path(getwd(), "source.R"))


# neither named ====

for(i in seq_along(funs)) {
 for(iDimX in c(TRUE, FALSE)) {
   op <- ops[[i]]
   x <- datagens[[i]]()
   y <- datagens[[i]]()
   if(iDimX) dim(x) <- length(x)
   dim(y) <- c(1, length(y))
   
   x.copy <- .rcpp_clone(x)
   y.copy <- .rcpp_clone(y)
   
   expect_equal(
     funs[[i]](x, y, op) |> names(),
     NULL
   ) |> errorfun()
   expect_equal(
     funs[[i]](y, x, op) |> names(),
     NULL
   ) |> errorfun()
   
   
   # check that original arrays remain unaffected:
   expect_equal(
     x, x.copy
   ) |> errorfun()
   expect_equal(
     y, y.copy
   ) |> errorfun()
   
   enumerate <- enumerate + 4L
 }
  

}



# only one side named ====

for(i in seq_along(funs)) {
  for(iDimX in c(TRUE, FALSE)) {
    for(iNamed in c(0, 1)) {
      
      x.nms <- NULL
      y.nms <- NULL
      
      op <- ops[[i]]
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      if(iDimX) dim(x) <- length(x)
      dim(y) <- c(1, length(y))
      
      if(iNamed == 0L) {
        names(x) <- sample(letters, length(x), TRUE)
        
        x.copy <- .rcpp_clone(x)
        y.copy <- .rcpp_clone(y)
        
        expect_equal(
          funs[[i]](x, y, op) |> dimnames(),
          list(names(x), NULL)
        ) |> errorfun()
        expect_equal(
          funs[[i]](y, x, op) |> dimnames(),
          list(names(x), NULL)
        ) |> errorfun()
        
        
        # check that original arrays remain unaffected:
        expect_equal(
          x, x.copy
        ) |> errorfun()
        expect_equal(
          y, y.copy
        ) |> errorfun()
        
        enumerate <- enumerate + 4L
        
      }
      if(iNamed == 1L) {
        dimnames(y) <- list("a", sample(letters, length(y), TRUE))
        
        x.copy <- .rcpp_clone(x)
        y.copy <- .rcpp_clone(y)
        
        expect_equal(
          funs[[i]](x, y, op) |> dimnames(),
          list(NULL, dimnames(y)[[2L]])
        ) |> errorfun()
        expect_equal(
          funs[[i]](y, x, op) |> dimnames(),
          list(NULL, dimnames(y)[[2L]])
        ) |> errorfun()
        
        
        # check that original arrays remain unaffected:
        expect_equal(
          x, x.copy
        ) |> errorfun()
        expect_equal(
          y, y.copy
        ) |> errorfun()
        
        enumerate <- enumerate + 4L
        
      }
    }
    
  }
  
  
}



# both sides reference same names ====

for(i in seq_along(funs)) {
  for(iDimX in c(TRUE, FALSE)) {
    op <- ops[[i]]
    
    x <- datagens[[i]]()
    y <- datagens[[i]]()
    if(iDimX) dim(x) <- length(x)
    dim(y) <- c(1, length(y))
    
    nms <- sample(letters, length(x), TRUE)
    names(x) <- nms
    dimnames(y) <- list(NULL, nms)
    
    x.copy <- .rcpp_clone(x)
    y.copy <- .rcpp_clone(y)
    
    expect_equal(
      funs[[i]](x, y, op) |> dimnames(),
      list(nms, nms)
    ) |> errorfun()
    expect_equal(
      funs[[i]](y, x, op) |> dimnames(),
      list(nms, nms)
    ) |> errorfun()
    
    
    # check that original arrays remain unaffected:
    expect_equal(
      x, x.copy
    ) |> errorfun()
    expect_equal(
      y, y.copy
    ) |> errorfun()
    
    enumerate <- enumerate + 4L
      
  }
}


# both sides have different names ====

for(i in seq_along(funs)) {
  for(iDimX in c(TRUE, FALSE)) {
    op <- ops[[i]]
    
    x <- datagens[[i]]()
    y <- datagens[[i]]()
    if(iDimX) dim(x) <- length(x)
    dim(y) <- c(1L, length(y))
    
    names(x) <- sample(letters, length(x), TRUE)
    dimnames(y) <- list("a", sample(letters, length(y), TRUE))
    
    x.copy <- .rcpp_clone(x)
    y.copy <- .rcpp_clone(y)
    
    expect_equal(
      funs[[i]](x, y, op) |> dimnames(),
      list(names(x), dimnames(y)[[2L]])
    ) |> errorfun()
    expect_equal(
      funs[[i]](y, x, op) |> dimnames(),
      list(names(x), dimnames(y)[[2L]])
    ) |> errorfun()
    
    
    # check that original arrays remain unaffected:
    expect_equal(
      x, x.copy
    ) |> errorfun()
    expect_equal(
      y, y.copy
    ) |> errorfun()
    
    enumerate <- enumerate + 4L
  }
}


