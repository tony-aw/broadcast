
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

source(file.path(getwd(), "source.R"))



for(i in seq_along(funs)) {
  
  op <- ops[[i]]
  
  mydimnames <- list(
    letters[1:3],
    LETTERS[1:3],
    c("x", "y", "z")
  )
  x <- array(datagens[[i]](), c(3,3,3), mydimnames)
  y <- datagens[[i]]()[0L]
  
  x.copy <- .rcpp_clone(x)
  y.copy <- .rcpp_clone(y)
  
  expect_equal(
    funs[[i]](x, y, op) |> dimnames(),
    NULL
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, op) |> dimnames(),
    NULL
  ) |> errorfun()
  
  dim(x) <- NULL
  dim(x.copy) <- NULL
  
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
  
  enumerate <- enumerate + 6L
  
}
