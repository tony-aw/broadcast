# test binary attributes
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

source(file.path(getwd(), "source.R"))

for(i in seq_along(funs)) {
  for(xBC in c(TRUE, FALSE)) {
    for(yBC in c(TRUE, FALSE)) {
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      broadcaster(x) <- xBC
      broadcaster(y) <- yBC
      
      out <- funs[[i]](x, y, ops[[i]])
      
      expect_equal(
        broadcaster(out),
        broadcaster(x) || broadcaster(y)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}

