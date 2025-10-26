
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
test_make_dimnames <- function(dims) {
  out <- lapply(dims, \(n) sample(letters, n, TRUE))
  indNULL <- sample(1:length(out), sample(1:length(out), 1L))
  out[indNULL] <- list(NULL)
  return(out)
}

coefun <- list(
  as.raw,
  as.logical,
  as.integer,
  as.double,
  as.complex,
  as.character
)



for(iSample in 1:5) {
  for(iCoe in 1:length(coefun)) {
    for(iLen in c(1, 6, 12)) {
      for(iDim in 0:3) {
        for(iNames in c(TRUE, FALSE)) {
          for(iDimNames in c(TRUE, FALSE)) {
            for(iFlatNames in c(TRUE, FALSE)) {
              for(iArr in c(1, -1)) {
                
                x <- lapply(1:iLen, \(i) 1:i)
                x <- lapply(x, coefun[[iCoe]])
                
                if(iNames) {
                  for(i in 1:length(x)) {
                    if(sample(c(TRUE, FALSE), 1L)) {
                      names(x[[i]]) <- sample(letters, length(x[[i]]), TRUE)
                    }
                  }
                }
                
                if(length(x) > 1) {
                  if(iDim == 1) dim(x) <- length(x)
                  if(iDim == 2) dim(x) <- c(3, length(x)/3)
                  if(iDim == 3) dim(x) <- c(3, 2, length(x)/6)
                }
                else {
                  if(iDim) dim(x) <- length(x)
                }
                
                if(length(x) > 1 && iDim && iDimNames) {
                  dimnames(x) <- test_make_dimnames(dim(x))
                }
                
                if(iFlatNames) names(x) <- sample(c("", month.name), length(x), TRUE)
                
                expect_silent(
                  cast_shallow2atomic(
                    x, iArr,
                    padding = x[[1]][1],
                    comnames_from = sample(1:length(x), 1L)
                  )
                ) |> errorfun()
                
                enumerate <- enumerate + 1L
                
              }
              
            }
          }
        }
      }
    }
  }
  
}


