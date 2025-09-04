
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


for(i in seq_along(funs)) {
  
  op <- ops[[i]]
  
  mydimnames <- list(
    letters[1:3],
    LETTERS[1:3],
    c("x", "y", "z")
  )
  x <- array(datagens[[i]](), c(3,3,3), mydimnames)
  y <- datagens[[i]]()[0L]
  
  expect_equal(
    funs[[i]](x, y, op) |> dimnames(),
    NULL
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, op) |> dimnames(),
    NULL
  ) |> errorfun()
  
  dim(x) <- NULL
  expect_equal(
    funs[[i]](x, y, op) |> names(),
    NULL
  ) |> errorfun()
  expect_equal(
    funs[[i]](y, x, op) |> names(),
    NULL
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
}
