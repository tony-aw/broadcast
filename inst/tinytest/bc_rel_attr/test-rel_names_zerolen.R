
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
  \() as.raw(sample(0:255, 10))
)


for(i in seq_along(datagens)) {
  mydimnames <- list(
    letters[1:3],
    LETTERS[1:3],
    c("x", "y", "z")
  )
  x <- array(datagens[[i]](), c(3,3,3), mydimnames)
  y <- datagens[[i]]()[0L]
  
  expect_equal(
    bc.rel(x, y, "==") |> dimnames(),
    NULL
  ) |> errorfun()
  expect_equal(
    bc.rel(y, x, "==") |> dimnames(),
    NULL
  ) |> errorfun()
  
  dim(x) <- NULL
  expect_equal(
    bc.rel(x, y, "==") |> names(),
    NULL
  ) |> errorfun()
  expect_equal(
    bc.rel(y, x, "==") |> names(),
    NULL
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
}

