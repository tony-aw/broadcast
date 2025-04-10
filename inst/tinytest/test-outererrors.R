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
  \(x, y) x == y
)

datagens <- list(
  # \() as.raw(sample(1:10)), # ifelse() cannot handle raw, apparently
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


# not array like error ====
message <- "input must be arrays or simple vecors"
for(i in seq_along(funs)) {
  x <- datagens[[i]]()
  expect_error(
    funs[[i]](x, as.factor(as.character(x)), ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](as.factor(as.character(x)), x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](as.factor(as.character(x)), as.factor(as.character(x)), ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}


# too many dimensions error ====
message <- "arrays with more than 16 dimensions are not supported"
for(i in seq_along(funs)) {
  x <- array(datagens[[i]](), dim = rep(2L, 17L))
  expect_error(
    funs[[i]](x, as.vector(x), ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](as.vector(x), x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](x, x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}


# zero-length object error ====
message <- "zero-length objects not supported"
for(i in seq_along(funs)) {
  x <- datagens[[i]]()
  zl <- vector(typeof(x), 0L)
  expect_error(
    funs[[i]](x, zl, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](zl, x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](zl, zl, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}


# op must be a single string ====
message <- "`op` must be single string"
op <- rep("==", 2L)
for(i in 1:5) {
  x <- datagens[[i]]()
  expect_error(
    funs[[i]](x, x, op),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}


# non-conformable vectors ====
message <- "`x` and `y` are not conformable"
for(i in seq_along(funs)) {
  
  x <- datagens[[i]]()
  y <- x[1:2]
  
  expect_error(
    funs[[i]](x, y, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](y, x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
}


# non-conformable arrays ====
message <- "`x` and `y` are not conformable"
for(i in seq_along(funs)) {
  
  x <- array(datagens[[i]](), c(2, 10))
  y <- array(datagens[[i]](), c(10, 2))
  
  expect_error(
    funs[[i]](x, y, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}


# broadcasting will exceed maximum size ====
maxint <- 2^53 + 1L
n <- ceiling(sqrt(maxint))
x <- array(c(TRUE, FALSE, NA), c(n, 1))
y <- array(c(TRUE, FALSE, NA), c(1, n))
expect_error(
  bc.b(x, y, "&"),
  pattern = "broadcasting will exceed maximum vector size"
)
enumerate <- enumerate + 1L



# operator errors ====
message <- "given operator not supported in the given context"
ops <- c(
  "+", "&", "&", "<", "<"
)
for(i in 1:5) {
  x <- datagens[[i]]()
  expect_error(
    funs[[i]](x, x, ops[i]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}

