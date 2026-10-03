
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen

# bit-wise negation for raw ====
x <- as.raw(sample(0:255))
expect_equal(
  !x,
  bc.bit(x, x, "nand")
)
x <- as.raw(sample(0:255))
expect_equal(
  !x,
  bc.bit(x, x, "nor")
)


# bit-wise negation for integer ====
x <- sample(0:100)
expect_equal(
  bitwNot(x),
  bc.bit(x, x, "nand")
)
x <- sample(0:100)
expect_equal(
  bitwNot(x),
  bc.bit(x, x, "nor")
)


enumerate <- enumerate + 4L

