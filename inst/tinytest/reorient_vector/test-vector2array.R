# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


for(iLen in seq(2, 100, 10L)) {
  for(iOrient in 1:16) {
    for(iNdim in seq(iOrient, 16L)) {
      for(iBc in c(TRUE, FALSE)) {
        for(iNamed in c(TRUE, FALSE)) {
          x <- sample(1:iLen)
          broadcaster(x) <- iBc
          if(iNamed) names(x) <- sample(month.abb, length(x), TRUE)
          
          out <- x
          out %orientbc<-% c(iOrient, iNdim)
          
          
          expect_equal(
            broadcaster(out), TRUE
          ) |> errorfun()
          
          expect_equal(
            names(x), dimnames(out)[[iOrient]]
          ) |> errorfun()
          
          expected.dim <- rep(1L, iNdim)
          expected.dim[iOrient] <- length(x)
          expect_equal(
            dim(out), expected.dim
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

