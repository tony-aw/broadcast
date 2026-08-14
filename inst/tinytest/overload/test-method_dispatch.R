
# set-up ====
enumerate <- 0L


# broadcasting works if at least one side is broadcaster ====
x <- array(1:10, c(1, 10))
broadcaster(x) <- TRUE
y <- 1:10

expect_equal(
  x + y,
  bc.d(x, y, "+")
)

expect_equal(
  dim(x + y),
  c(10, 10)
)

expect_equal(
  y + x,
  bc.d(y, x, "+")
)

expect_equal(
  dim(y + x),
  c(10, 10)
)

enumerate <- enumerate + 4L



# conflicting class overwrites broadcaster ====

`+.myclass` <- function(e1, e2) {
  return("overwrite dispatch")
}
chooseOpsMethod.myclass <- function(x, y, mx, my, cl, reverse) TRUE

x <- array(1:10, c(1, 10))
broadcaster(x) <- TRUE
y <- 1:10
class(y) <- "myclass"

expect_equal(
  x + y,
  "overwrite dispatch"
)

expect_equal(
  y + x,
  "overwrite dispatch"
)

enumerate <- enumerate + 2L

rm("+.myclass", "chooseOpsMethod.myclass")
