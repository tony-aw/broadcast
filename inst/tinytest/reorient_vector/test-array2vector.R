# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


for(iLen in seq(2, 100, 10L)) {
  for(iOrient in 1:16) {
    for(iNdim in seq(iOrient, 16L)) {
      for(iBc in c(TRUE, FALSE)) {
        for(iNamed in c(TRUE, FALSE, NA)) {
          
          x.orient <- sample(1:iNdim, 1L)
          x.dim <- rep(1L, iNdim)
          x.dim[x.orient] <- iLen
          if(is.na(iNamed)) {
            x.dimnames <- rep(NULL, iNdim)
          }
          else if(iNamed) {
            x.dimnames <- rep(NULL, iNdim)
            x.dimnames[[x.orient]] <- sample(month.abb, iLen, TRUE)
          }
          else if(!iNamed) {
            x.dimnames <- NULL
          }
          
          x <- array(sample(1:iLen), x.dim, x.dimnames)
          broadcaster(x) <- iBc
          
          out <- x
          out %orientbc<-% c(1L, 1L)
          
          
          expect_equal(
            broadcaster(out), TRUE
          ) |> errorfun()
          
          expect_equal(
            dimnames(x)[[x.orient]], dimnames(out)[[1L]]
          ) |> errorfun()
          
          expect_equal(
            dim(out), iLen
          ) |> errorfun()
          
          expect_equal(
            as.vector(x),
            as.vector(out)
          ) |> errorfun()
          
          enumerate <- enumerate + 4L
          
        }
      }
    }
  }
}

