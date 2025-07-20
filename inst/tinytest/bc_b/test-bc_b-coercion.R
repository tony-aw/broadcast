
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

ops <- broadcast:::.op_b()

for(op in ops) {
  x <- array(sample(c(-49:49, NA)), c(100, 1))
  y <- array(sample(c(-49:49, NA)), c(1, 100))
  expect_equal(
    bc.b(as_dbl(x), as_dbl(y), op),
    bc.b(x, y, op)
  ) |> errorfun()
  enumerate <- enumerate + 1
}


