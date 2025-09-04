
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


# neither named ====

for(i in seq_along(datagens)) {
 for(iDimX in c(TRUE, FALSE)) {
   
   x <- datagens[[i]]()
   y <- datagens[[i]]()
   if(iDimX) dim(x) <- length(x)
   dim(y) <- c(1, length(y))
   
   expect_equal(
     bcapply(x, y, \(x, y) paste0(x, y)) |> names(),
     NULL
   ) |> errorfun()
   expect_equal(
     bcapply(y, x, \(x, y) paste0(x, y)) |> names(),
     NULL
   ) |> errorfun()
   
   enumerate <- enumerate + 2L
 }
  

}



# only one side named ====

for(i in seq_along(datagens)) {
  for(iDimX in c(TRUE, FALSE)) {
    for(iNamed in c(0, 1)) {
      
      x.nms <- NULL
      y.nms <- NULL
      
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      if(iDimX) dim(x) <- length(x)
      dim(y) <- c(1, length(y))
      
      if(iNamed == 0L) {
        names(x) <- sample(letters, length(x), TRUE)
        expect_equal(
          bcapply(x, y, \(x, y) paste0(x, y)) |> dimnames(),
          list(names(x), NULL)
        ) |> errorfun()
        expect_equal(
          bcapply(y, x, \(x, y) paste0(x, y)) |> dimnames(),
          list(names(x), NULL)
        ) |> errorfun()
        
        enumerate <- enumerate + 2L
        
      }
      if(iNamed == 1L) {
        dimnames(y) <- list("a", sample(letters, length(y), TRUE))
        expect_equal(
          bcapply(x, y, \(x, y) paste0(x, y)) |> dimnames(),
          list(NULL, dimnames(y)[[2L]])
        ) |> errorfun()
        expect_equal(
          bcapply(y, x, \(x, y) paste0(x, y)) |> dimnames(),
          list(NULL, dimnames(y)[[2L]])
        ) |> errorfun()
        
        enumerate <- enumerate + 2L
        
      }
    }
    
  }
  
  
}



# both sides reference same names ====

for(i in seq_along(datagens)) {
  for(iDimX in c(TRUE, FALSE)) {
    
    
    x <- datagens[[i]]()
    y <- datagens[[i]]()
    if(iDimX) dim(x) <- length(x)
    dim(y) <- c(1, length(y))
    
    nms <- sample(letters, length(x), TRUE)
    names(x) <- nms
    dimnames(y) <- list(NULL, nms)
    
    
    expect_equal(
      bcapply(x, y, \(x, y) paste0(x, y)) |> dimnames(),
      list(nms, nms)
    ) |> errorfun()
    expect_equal(
      bcapply(y, x, \(x, y) paste0(x, y)) |> dimnames(),
      list(nms, nms)
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
      
  }
}


# both sides have different names ====

for(i in seq_along(datagens)) {
  for(iDimX in c(TRUE, FALSE)) {
    
    
    x <- datagens[[i]]()
    y <- datagens[[i]]()
    if(iDimX) dim(x) <- length(x)
    dim(y) <- c(1L, length(y))
    
    names(x) <- sample(letters, length(x), TRUE)
    dimnames(y) <- list("a", sample(letters, length(y), TRUE))
    
    expect_equal(
      bcapply(x, y, \(x, y) paste0(x, y)) |> dimnames(),
      list(names(x), dimnames(y)[[2L]])
    ) |> errorfun()
    expect_equal(
      bcapply(y, x, \(x, y) paste0(x, y)) |> dimnames(),
      list(names(x), dimnames(y)[[2L]])
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
  }
}


