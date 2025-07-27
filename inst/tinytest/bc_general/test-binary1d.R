# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

undim <- function(x) {
  dim(x) <- NULL
  return(x)
}

# bcapply ====

f <- function(x, y) x == y

x <- array(1:10, c(10, 1))
y <- array(1:10, c(1, 10))
expect_equal(
  bcapply(x, y, f),
  bcapply(as.vector(x), y, f)
)
expect_equal(
  bcapply(y, x, f),
  bcapply(y, as.vector(x), f)
)



x <- array(sample(1:10), 10L)
y <- sample(1:10)
expect_equal(
  dim(bcapply(x, y, f)),
  10L
)
expect_equal(
  dim(bcapply(y, x, f)),
  10L
)
expect_equal(
  dim(bcapply(undim(x), y, f)),
  NULL
)
expect_equal(
  dim(bcapply(y, undim(x), f)),
  NULL
)

enumerate <- enumerate + 6L



# ifelse ====
x <- array(1:10, c(10, 1))
y <- array(1:10, c(1, 10))

cond <- bc.b(x, y, "==")
expect_equal(
  bc_ifelse(cond, x, y),
  bc_ifelse(cond, as.vector(x), y)
)

cond <- bc.b(y, x, "==")
expect_equal(
  bc_ifelse(cond, y, x),
  bc_ifelse(cond, y, as.vector(x))
)

x <- array(sample(1:10), 10L)
y <- sample(1:10)

cond <- bc.i(x, y, ">")
expect_equal(
  dim(bc_ifelse(cond, x, y)),
  10L
)
expect_equal(
  dim(bc_ifelse(cond, y, x)),
  10L
)

cond <- bc.i(x, y, ">")
expect_equal(
  dim(bc_ifelse(cond, undim(x), y)),
  NULL
)
expect_equal(
  dim(bc_ifelse(cond, y, undim(x))),
  NULL
)
enumerate <- enumerate + 6L

