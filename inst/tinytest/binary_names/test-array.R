
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dimnames <- function(dims) {
  out <- lapply(dims, \(n) sample(letters, n, TRUE))
  indNULL <- sample(1:length(out), sample(1:length(out), 1L))
  out[indNULL] <- list(NULL)
  return(out)
}
test_make_dims <- function(ndim) {
  return(sample(c(1, 5), ndim, TRUE))
}
test_dimnames <- function(x.dimnames, y.dimnames, out.dimnames) {
  
  nx <- length(x.dimnames)
  ny <- length(y.dimnames)
  nout <- length(out.dimnames)
  res <- logical(nout)
  
  if(nx != nout && !is.null(x.dimnames)) {
    x.dimnames <- c(x.dimnames, rep(list(NULL), nout - nx))
  }
  if(ny != nout && !is.null(y.dimnames)) {
    y.dimnames <- c(y.dimnames, rep(list(NULL), nout - ny))
  }
  
  for(i in seq_along(out.dimnames)) {
    res[i] <- all(out.dimnames[[i]] == x.dimnames[[i]]) || all(out.dimnames[[i]] == y.dimnames[[i]])
  }
  return(all(res))
}

OK2test <- function(x.dimnames, y.dimnames) {
  checkx <- !is.null(x.dimnames) && broadcast:::.C_any_nonNULL(x.dimnames)
  checky <- !is.null(y.dimnames) && broadcast:::.C_any_nonNULL(y.dimnames)
  return(checkx || checky)
}


source(file.path(getwd(), "source.R"))


# tests - x and y distinct ===
res <- rep(FALSE, length(funs) * 5 * 5 * 2 * 2 * 5)
counter <- 1L

for(i in seq_along(funs)) {
 for(iDimX in 1:5) {
   for(iDimY in 1:5) {
     for(iDimNmsX in c(TRUE, FALSE)) {
       for(iDimNmsY in c(TRUE, FALSE)) {
         for(iSample in 1:5) {
           
           op <- ops[[i]]
           
           x.dim <- test_make_dims(iDimX)
           x.dimnames <- NULL
           if(iDimNmsX) x.dimnames <- test_make_dimnames(x.dim)
           x <- array(datagens[[i]](), x.dim, x.dimnames)
           
           y.dim <- test_make_dims(iDimY)
           y.dimnames <- NULL
           if(iDimNmsY) y.dimnames <- test_make_dimnames(y.dim)
           y <- array(datagens[[i]](), y.dim, y.dimnames)
           
           out <- funs[[i]](x, y, op) 
           
           if(!is.null(dimnames(out))) {
            res[counter] <- test_dimnames(x.dimnames, y.dimnames, dimnames(out))
             
           }
           else{
             res[counter] <- is.null(dimnames(out))
           }
           enumerate <- enumerate + 1L
           counter <- counter + 1L
         }
       }
     }
   }
 }
}
expect_true(
  all(res)
)


# tests - x and y dimnames share (some) references ====
res <- rep(FALSE, length(funs) * 5 * 2 * 2 * 5)
counter <- 1L

for(i in seq_along(funs)) {
  for(iDim in 1:5) {
    for(iDimNms in c(TRUE, FALSE)) {
      for(iChange1 in c(TRUE, FALSE)) {
        for(iSample in 1:5) {
          
          op <- ops[[i]]
          
          dims <- test_make_dims(iDimX)
          dimnames <- NULL
          if(iDimNms) dimnames <- test_make_dimnames(dims)
          x <- array(datagens[[i]](), dims, dimnames)
          y <- array(datagens[[i]](), dims, dimnames)
          
          if(iChange1 && iDimNms) {
            ind <- sample(1:length(dims), 1L)
            dimnames(y)[ind] <- list(sample(month.abb, dims[ind], TRUE))
          }
          
          out <- funs[[i]](x, y, op) 
          
          if(!is.null(dimnames(out))) {
            res[counter] <- test_dimnames(dimnames(x), dimnames(y), dimnames(out))
            
          }
          else{
            res[counter] <- is.null(dimnames(out))
          }
          enumerate <- enumerate + 1L
          counter <- counter + 1L
        }
      }
    }
  }
}

expect_true(
  all(res)
)
