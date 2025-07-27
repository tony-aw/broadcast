
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

undim <- function(x) {
  dim(x) <- NULL
  return(x)
}

funs <- list(
  bc.b,
  bc.i,
  bc.d,
  bc.cplx,
  bc.str,
  bc.raw,
  bc.bit,
  bc.list
)
funs <- c(funs, funs)
ops1 <- c(
  rep(list("=="), 7L),
  \(x, y) length(x)==length(y)
)
ops2 <- c(
  list("&"), rep(list("+"), 4L), list("pmin", "&"),
  \(x, y) length(x)==length(y)
)
ops <- c(ops1, ops2)

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(0:255, 10)),
  \() as.raw(sample(0:255, 10)),
  \() sample(list(letters, month.abb, 1:10))
)
datagens <- c(datagens, datagens)


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


