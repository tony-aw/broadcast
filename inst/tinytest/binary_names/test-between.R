
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
test_dimnames <- function(x.names, y.dimnames, out.dimnames) {
  
  ny <- length(y.dimnames)
  nout <- length(out.dimnames)
  res <- logical(nout)
  
  if(ny != nout) {
    y.dimnames <- c(y.dimnames, rep(list(NULL), nout - ny))
  }
  
  for(i in seq_along(out.dimnames)) {
    if(i == 1L) {
      res[i] <- all(out.dimnames[[i]] == x.names) || all(out.dimnames[[i]] == y.dimnames[[i]])
    }
    else {
      res[i] <- all(out.dimnames[[i]] == y.dimnames[[i]])
    }
    
  }
  return(all(res))
}


source(file.path(getwd(), "source.R"))



# tests ===
res <- rep(FALSE, length(funs) * 2 * 5 * 2 * 2 * 5)
counter <- 1L

for(i in seq_along(funs)) {
  for(iLenX in c(1, 5)) {
   for(iDimY in 1:5) {
     for(iNmsX in c(TRUE, FALSE)) {
       for(iDimNmsY in c(TRUE, FALSE)) {
         for(iSample in 1:5) {
           
           op <- ops[[i]]
           
           x <- datagens[[i]]()[1:iLenX]
           if(iNmsX) names(x) <- sample(letters, length(x))
           
           y.dim <- test_make_dims(iDimY)
           y.dimnames <- NULL
           if(iDimNmsY) y.dimnames <- test_make_dimnames(y.dim)
           y <- array(datagens[[i]](), y.dim, y.dimnames)
           
           out <- funs[[i]](x, y, op) 
           
           if(!is.null(dimnames(out))) {
             res[counter] <- test_dimnames(names(x), y.dimnames, dimnames(out))
             
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


res <- rep(FALSE, length(funs) * 2 * 5 * 2 * 2 * 5)
counter <- 1L
for(i in seq_along(funs)) {
  for(iLenX in c(1, 5)) {
    for(iDimY in 1:5) {
      for(iNmsX in c(TRUE, FALSE)) {
        for(iDimNmsY in c(TRUE, FALSE)) {
          for(iSample in 1:5) {
            
            op <- ops[[i]]
            
            x <- datagens[[i]]()[1:iLenX]
            if(iNmsX) names(x) <- sample(letters, length(x))
            
            y.dim <- test_make_dims(iDimY)
            y.dimnames <- NULL
            if(iDimNmsY) y.dimnames <- test_make_dimnames(y.dim)
            y <- array(datagens[[i]](), y.dim, y.dimnames)
            
            out <- funs[[i]](y, x, op) # x and y reversed compared to previous test
            
            if(!is.null(dimnames(out))) {
              res[counter] <- test_dimnames(names(x), y.dimnames, dimnames(out))
              
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


