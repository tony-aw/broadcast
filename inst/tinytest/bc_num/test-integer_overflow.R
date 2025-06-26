
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# overflow ====
expect_equal(
  bc.i(2^53, 1, "+") |> drop(),
  Inf
)

expect_equal(
  bc.i(-2^53, 1, "-") |> drop(),
  -Inf
)
enumerate <- enumerate + 2L

