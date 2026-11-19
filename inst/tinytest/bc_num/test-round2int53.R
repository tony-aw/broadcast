
# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
eps <- .Machine$double.eps


# check basic truncation ====

offset <- rep_len(c(1e-12, -1e-12 ), 100)
x <- seq(-10, 10, length.out = 100)
x <- x + offset
expect_equal(
  round(x), round2int53(x)
)
expect_true(
  all(bc.i(x, round(x), "=="))
)

x <- seq(-2^16 + 1, 2^16 -1 , length.out = 100)
x <- x + offset
expect_equal(
  round(x), round2int53(x)
)
expect_true(
  all(bc.i(x, round(x), "=="))
)


x <- seq(-2^31 + 1, 2^31 -1 , length.out = 100)
x <- x + offset
expect_equal(
  round(x), round2int53(x)
)
expect_true(
  all(bc.i(x, round(x), "=="))
)


x <- seq(-2^52 + 1, 2^52 -1 , length.out = 100)
x <- x + offset
expect_equal(
  round(x), round2int53(x)
)

x <- seq(-2^53 + 1, 2^53 -1 , length.out = 100)
expect_equal(
  round(x), round2int53(x)
)
expect_true(
  all(bc.i(x, round(x), "=="))
)


enumerate <- enumerate + 10L


# basic functionality ====

testfun <- function(x) {
  intmax <- 2^53 - 1
  intmin <- intmax * -1
  y <- round(x)
  y[which(y > intmax)] <- Inf
  y[which(y < intmin)] <- -Inf
  return(y)
}

x <- sample(c(-Inf, Inf, NA, NaN, -2^54, 2^54, seq(-100, 100, by = 10.1)), 100, TRUE)
expect_equal(
  testfun(x), round2int53(x)
)

x <- sample(c(-2^30, 2^30, NA, -100:100))
expect_equal(
  testfun(x), round2int53(x)
)

x <- sample(c(TRUE, FALSE, NA), 100, TRUE)
expect_equal(
  testfun(x) |> as.logical(), round2int53(x)
)

x <- double(0L)
expect_equal(
  testfun(x), round2int53(x)
)

enumerate <- enumerate + 4L


# attrributes ====
x <- sample(c(-Inf, Inf, NA, NaN, -2^54, 2^54, seq(-100, 100, by = 10.1)), 100, TRUE)
dim(x) <- c(20, 5)
dimnames(x) <- list(letters[1:20], month.abb[1:5])
names(x) <- sample(LETTERS, length(x), TRUE)
comment(x) <- "testcomment"
class(x) <- "testclass"
attr(x, "myattr") <- "testattr"

expect_equal(
  testfun(x), round2int53(x)
)
enumerate <- enumerate + 1L


# errors ====
x <- as.raw(0:255)
expect_error(
  round2int53(x),
  pattern = "unsupported type given"
)
enumerate <- enumerate + 1L
