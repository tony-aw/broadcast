# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

f <- function(x, y) x == y

x <- array(1:10, c(10, 1))
y <- array(1:10, c(1, 10))
cond <- bc.b(x, y, "==")
expect_equal(
  bcapply(x, y, f),
  bcapply(as.vector(x), y, f)
)
expect_equal(
  bcapply(y, x, f),
  bcapply(y, as.vector(x), f)
)
expect_equal(
  bc_ifelse(cond, x, y),
  bc_ifelse(cond, as.vector(x), y)
)
expect_equal(
  bc_ifelse(cond, y, x),
  bc_ifelse(cond, y, as.vector(x))
)
enumerate <- enumerate + 4L


