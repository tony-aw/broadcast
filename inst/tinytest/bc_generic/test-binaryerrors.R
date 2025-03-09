# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

f <- function(x, y) x == y


# not array like error ====
message <- "input must be arrays or simple vecors"
x <- 1:10
expect_error(
  bc_ifelse(x == x, x, as.factor(x)),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, as.factor(x), x),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, as.factor(x), as.factor(x)),
  pattern = message
)
expect_error(
  bcapply(x, as.factor(x), f),
  pattern = message
)
expect_error(
  bcapply(as.factor(x), x, f),
  pattern = message
)
expect_error(
  bcapply(as.factor(x), as.factor(x), f),
  pattern = message
)
enumerate <- enumerate + 6L



# too many dimensions error ====
message <- "arrays with more than 16 dimensions are not supported"
x <- array(1:10, dim = rep(2L, 17L))
expect_error(
  bc_ifelse(x == x, x, as.vector(x)),
  pattern = message
)

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

