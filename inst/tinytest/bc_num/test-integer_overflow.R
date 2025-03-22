
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# overflow ====
expect_equal(
  bc.i(2^54, 1, "+") |> drop(),
  Inf
)

expect_equal(
  bc.i(-2^54, 1, "-") |> drop(),
  -Inf
)
enumerate <- enumerate + 2L

