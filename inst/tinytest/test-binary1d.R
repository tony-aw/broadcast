# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



funs <- list(
  bc.b,
  bc.i,
  bc.d,
  bc.cplx,
  bc.str,
  bc.list
)
ops <- c(
  rep(list("=="), 5L),
  \(x, y) length(x)==length(y)
)

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


for(i in seq_along(funs)) {
  x <- array(datagens[[i]](), c(10, 1))
  y <- array(datagens[[i]](), c(1, 10))
  expect_equal(
    funs[[i]](x, y, ops[[i]]),
    funs[[i]](as.vector(x), y, ops[[i]])
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, ops[[i]]),
    funs[[i]](y, as.vector(x), ops[[i]])
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}

