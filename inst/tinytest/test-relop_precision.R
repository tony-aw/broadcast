
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

prec <- sqrt(.Machine$double.eps)


# basic tests ====
x <- as.array(c(1:100, NA, NaN, Inf, -Inf))
y <- as.array(c(sample(1:100), NA, NaN, Inf, -Inf))
expect_equal(
  bc(x, y, "==") |> as.vector(),
  as.vector(x == y)
)
expect_equal(
  bc(x, y, "!=") |> as.vector(),
  as.vector(x != y)
)
expect_equal(
  bc(x, y, "==") |> as.vector(),
  as.vector(x == y)
)
