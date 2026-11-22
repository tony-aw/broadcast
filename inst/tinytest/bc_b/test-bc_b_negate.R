
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen

# logical negation ===
x <- c(TRUE, FALSE, NA)
expect_equal(
  !x,
  bc.b(x, x, "nand")
)
expect_equal(
  !x,
  bc.b(x, x, "nor")
)

x <- as.raw(c(0, 1))
expect_equal(
  bc.b(x, x, "nand"),
  as.raw(c(1,0))
)
expect_equal(
  bc.b(x, x, "nor"),
  as.raw(c(1,0))
)
enumerate <- enumerate + 4L

