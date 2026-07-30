
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.binary_prep <- function(x, y) {
  
  x.ndim <- ndim(x)
  y.ndim <- ndim(y)
  out <- broadcast:::.rcpp_virt_binary_prep(x, y, x.ndim, y.ndim)
  names(out) <- c("x.dim", "y.dim", "out.dimorig", "out.dimsimp", "out.len", "dimmode")
  return(out)
  
}


# array by array

for(i in 1:100) {
  for(iNdimX in 1:8) {
    x.dim <- sample(c(1, 5), iNdimX, replace = TRUE)
    x.dim2 <- broadcast:::.rcpp_clone(x.dim)
    for(iNdimY in 1:8) {
      y.dim <- sample(c(1, 5), iNdimY, replace = TRUE)
      y.dim2 <- broadcast:::.rcpp_clone(y.dim)
      
      x <- array(rnorm(10), x.dim)
      y <- array(rnorm(10), y.dim)
      
      .binary_prep(x, y)
      expect_equal(
        x.dim, x.dim2
      ) |> errorfun()
      
      expect_equal(
        y.dim, y.dim2
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
      
    }
  }
}



# vector by vector

for(i in 1:100) {
  
  x <- 1:20
  y <- 1:20
  x.dim <- dim(x)
  x.dim2 <- broadcast:::.rcpp_clone(x.dim)
  
  y.dim <- dim(y)
  y.dim2 <- broadcast:::.rcpp_clone(y.dim)
  
  .binary_prep(x, y)
  expect_equal(
    x.dim, x.dim2
  ) |> errorfun()
  
  expect_equal(
    y.dim, y.dim2
  ) |> errorfun()
  
  enumerate <- enumerate + 2L

}



# vector by array

for(i in 1:100) {
  
  x <- 1:20
  y <- array(1:20, c(1, 20))
  x.dim <- dim(x)
  x.dim2 <- broadcast:::.rcpp_clone(x.dim)
  
  y.dim <- dim(y)
  y.dim2 <- broadcast:::.rcpp_clone(y.dim)
  
  .binary_prep(x, y)
  expect_equal(
    x.dim, x.dim2
  ) |> errorfun()
  
  expect_equal(
    y.dim, y.dim2
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}


# array by vector

for(i in 1:100) {
  
  x <- array(1:20, c(1, 20))
  y <- 1:20
  x.dim <- dim(x)
  x.dim2 <- broadcast:::.rcpp_clone(x.dim)
  
  y.dim <- dim(y)
  y.dim2 <- broadcast:::.rcpp_clone(y.dim)
  
  .binary_prep(x, y)
  expect_equal(
    x.dim, x.dim2
  ) |> errorfun()
  
  expect_equal(
    y.dim, y.dim2
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}


