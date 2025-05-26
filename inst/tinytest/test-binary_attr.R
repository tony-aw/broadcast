# test binary attributes
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

funs <- list(
  # not bc.b, bc.bit, and bc.list, as they don't have to keep attributes
  bc.i,
  bc.d,
  bc.cplx,
  bc.str,
  bc.raw
)
datagens <- list(
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(1:10))
)


ops <- c(rep("+", 4), "diff")

for(i in seq_along(funs)) {
  for(xBC in c(TRUE, FALSE)) {
    for(yBC in c(TRUE, FALSE)) {
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      broadcaster(x) <- xBC
      broadcaster(y) <- yBC
      
      out <- funs[[i]](x, y, ops[i])
      
      expect_equal(
        broadcaster(out),
        broadcaster(x) || broadcaster(y)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}

