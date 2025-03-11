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
y <- as.factor(x)
expect_error(
  bc_ifelse(x == x, x, y),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, y, x),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, y, y),
  pattern = message
)
expect_error(
  bcapply(x, y, f),
  pattern = message
)
expect_error(
  bcapply(y, x, f),
  pattern = message
)
expect_error(
  bcapply(y, y, f),
  pattern = message
)
enumerate <- enumerate + 6L



# too many dimensions error ====
message <- "arrays with more than 16 dimensions are not supported"
x <- array(1:10, dim = rep(2L, 17L))
y <- as.vector(x)
expect_error(
  bc_ifelse(x == x, x, y),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, y, x),
  pattern = message
)
expect_error(
  bcapply(x, y, f),
  pattern = message
)
expect_error(
  bcapply(y, x, f),
  pattern = message
)
enumerate <- enumerate + 4L



# zero-length object error ====
message <- "zero-length objects not supported"
x <- array(1:27, c(3,3,3))
y <- vector(typeof(x), 0L)
expect_error(
  bc_ifelse(x == x, x, y),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, y, x),
  pattern = message
)
expect_error(
  bcapply(x, y, f),
  pattern = message
)
expect_error(
  bcapply(y, x, f),
  pattern = message
)
enumerate <- enumerate + 4L



# non-conformable vectors ====
message <- "`x` and `y` are not conformable"
x <- 1:10
y <- x[1:2]
expect_error(
  bc_ifelse(x == x, x, y),
  pattern = message
)
expect_error(
  bc_ifelse(x == x, y, x),
  pattern = message
)
expect_error(
  bcapply(x, y, f),
  pattern = message
)
expect_error(
  bcapply(y, x, f),
  pattern = message
)
enumerate <- enumerate + 4L


# non-conformable arrays ====
message <- "`x` and `y` are not conformable"
x <- array(1:10, c(2, 10))
y <- array(1:10, c(10, 2))
cond <- array(logical(10), c(10,10))
expect_error(
  bc_ifelse(cond, x, y),
  pattern = message
)
expect_error(
  bc_ifelse(cond, y, x),
  pattern = message
)
expect_error(
  bcapply(x, y, f),
  pattern = message
)
expect_error(
  bcapply(x, y, f),
  pattern = message
)
enumerate <- enumerate + 4L


# broadcasting will exceed maximum size ====
maxint <- 2^53 + 1L
n <- ceiling(sqrt(maxint))
x <- array(c(TRUE, FALSE, NA), c(n, 1))
y <- array(c(TRUE, FALSE, NA), c(1, n))
expect_error(
  bcapply(x, y, f),
  pattern = "broadcasting will exceed maximum vector size"
)
enumerate <- enumerate + 1L

