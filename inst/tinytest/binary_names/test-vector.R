
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

source(file.path(getwd(), "source.R"))



# neither named ====

for(i in seq_along(funs)) {
 
  op <- ops[[i]]
  x <- datagens[[i]]()
  y <- datagens[[i]]()
  
  x.copy <- .rcpp_clone(x)
  y.copy <- .rcpp_clone(y)
  
  expect_equal(
    funs[[i]](x, y, op) |> names(),
    NULL
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, op) |> names(),
    NULL
  ) |> errorfun()
  
  
  # check that original arrays remain unaffected:
  expect_equal(
    x, x.copy
  ) |> errorfun()
  expect_equal(
    y, y.copy
  ) |> errorfun()
  
  enumerate <- enumerate + 4L

}



# both sides reference same names ====

for(i in seq_along(funs)) {
  op <- ops[[i]]
  
  x <- datagens[[i]]()
  y <- datagens[[i]]()
  nms <- sample(letters, length(x), TRUE)
  names(x) <- nms
  names(y) <- nms
  
  x.copy <- .rcpp_clone(x)
  y.copy <- .rcpp_clone(y)
  
  expect_equal(
    funs[[i]](x, y, op) |> names(),
    nms
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, op) |> names(),
    nms
  ) |> errorfun()
  
  
  # check that original arrays remain unaffected:
  expect_equal(
    x, x.copy
  ) |> errorfun()
  expect_equal(
    y, y.copy
  ) |> errorfun()
  
  enumerate <- enumerate + 4L

  
}


# both sides have different names ====

for(i in seq_along(funs)) {
  op <- ops[[i]]
  
  x <- datagens[[i]]()
  y <- datagens[[i]]()
  names(x) <- sample(letters, length(x), TRUE)
  names(y) <- sample(letters, length(y), TRUE)
  
  x.copy <- .rcpp_clone(x)
  y.copy <- .rcpp_clone(y)
  
  expect_equal(
    funs[[i]](x, y, op) |> names(),
    names(x)
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, op) |> names(),
    names(y)
  ) |> errorfun()
  
  
  # check that original arrays remain unaffected:
  expect_equal(
    x, x.copy
  ) |> errorfun()
  expect_equal(
    y, y.copy
  ) |> errorfun()
  
  enumerate <- enumerate + 4L

}


# only one of the sides' names should be used ====

for(i in seq_along(funs)) {
  for(iSameLen in c(TRUE, FALSE)) {
    for(iNamed in c(TRUE, FALSE)) {
      op <- ops[[i]]
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
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
      
      
      x.copy <- .rcpp_clone(x)
      y.copy <- .rcpp_clone(y)
      
      expect_equal(
        funs[[i]](x, y, op) |> names(),
        nms
      ) |> errorfun()
      expect_equal(
        funs[[i]](y, x, op) |> names(),
        nms
      ) |> errorfun()
      
      
      # check that original arrays remain unaffected:
      expect_equal(
        x, x.copy
      ) |> errorfun()
      expect_equal(
        y, y.copy
      ) |> errorfun()
      
      enumerate <- enumerate + 4L
    }
  }
}


