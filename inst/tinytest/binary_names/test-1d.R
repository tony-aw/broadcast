
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
   for(iDimY in c(TRUE, FALSE)) {
     op <- ops[[i]]
     x <- datagens[[i]]()
     y <- datagens[[i]]()
     if(iDimX) dim(x) <- length(x)
     if(iDimY) dim(y) <- length(y)
     
     expect_equal(
       funs[[i]](x, y, op) |> names(),
       NULL
     ) |> errorfun()
     expect_equal(
       funs[[i]](y, x, op) |> names(),
       NULL
     ) |> errorfun()
     
     enumerate <- enumerate + 2L
   }
 }
  

}



# both sides reference same names ====

for(i in seq_along(funs)) {
  for(iDimX in c(TRUE, FALSE)) {
    for(iDimY in c(TRUE, FALSE)) {
      op <- ops[[i]]
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      if(iDimX) dim(x) <- length(x)
      if(iDimY) dim(y) <- length(y)
      
      nms <- sample(letters, length(x), TRUE)
      names(x) <- nms
      names(y) <- nms
      
      
      expect_equal(
        funs[[i]](x, y, op) |> names(),
        nms
      ) |> errorfun()
      expect_equal(
        funs[[i]](y, x, op) |> names(),
        nms
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
      
    }
  }
}


# both sides have different names ====

for(i in seq_along(funs)) {
  for(iDimX in c(TRUE, FALSE)) {
    for(iDimY in c(TRUE, FALSE)) {
      op <- ops[[i]]
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      if(iDimX) dim(x) <- length(x)
      if(iDimY) dim(y) <- length(y)
      
      names(x) <- sample(letters, length(x), TRUE)
      names(y) <- sample(letters, length(y), TRUE)
      
      expect_equal(
        funs[[i]](x, y, op) |> names(),
        NULL
      ) |> errorfun()
      expect_equal(
        funs[[i]](y, x, op) |> names(),
        NULL
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
    }
  }
  
}


# only one of the sides' names should be used ====

for(i in seq_along(funs)) {
  for(iSameLen in c(TRUE, FALSE)) {
    for(iNamed in c(TRUE, FALSE)) {
      for(iDimX in c(TRUE, FALSE)) {
        for(iDimY in c(TRUE, FALSE)) {
              
          op <- ops[[i]]
          x <- datagens[[i]]()
          y <- datagens[[i]]()
          if(iDimX) dim(x) <- length(x)
          if(iDimY) dim(y) <- length(y)
          
          if(iSameLen == FALSE) {
            y <- y[1]
          }
          if(iNamed == TRUE) {
            names(y) <- sample(letters, length(y), TRUE)
          }
          if(length(x) == length(y)) {
            names(y) <- NULL
          }
          
          nms <- sample(letters, length(x), TRUE)
          names(x) <- nms
          
          
          expect_equal(
            funs[[i]](x, y, op) |> names(),
            nms
          ) |> errorfun()
          expect_equal(
            funs[[i]](y, x, op) |> names(),
            nms
          ) |> errorfun()
          
          enumerate <- enumerate + 2L
        }
      }
    }
  }
}


