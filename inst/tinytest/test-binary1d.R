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
source(file.path(getwd(), "source.R"))

for(i in seq_along(funs)) {
  x <- array(datagens[[i]](), c(10, 1))
  y <- array(datagens[[i]](), c(1, 10))
  expect_equal(
    funs[[i]](x, y, ops[[i]]),
    funs[[i]](undim(x), y, ops[[i]])
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, ops[[i]]),
    funs[[i]](y, undim(x), ops[[i]])
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}


for(i in seq_along(funs)) {
  x <- array(datagens[[i]](), 10L)
  y <- datagens[[i]]()[1:10]
  expect_equal(
    dim(funs[[i]](x, y, ops[[i]])),
    10L
  ) |> errorfun()
  expect_equal(
    dim(funs[[i]](y, x, ops[[i]])),
    10L
  ) |> errorfun()
  expect_equal(
    dim(funs[[i]](undim(x), y, ops[[i]])),
    NULL
  ) |> errorfun()
  expect_equal(
    dim(funs[[i]](y, undim(x), ops[[i]])),
    NULL
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
}
  


