# test binary attributes
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

funs <- list(
  bc.b,
  bc.i,
  bc.d,
  bc.cplx,
  bc.str,
  bc.raw,
  bc.bit,
  bc.list
)
funs <- c(funs, funs)

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(1:10)),
  \() as.raw(sample(1:10)),
  \() as.list(sample(-10:10))
)
datagens <- c(datagens, datagens)

ops1 <- c(list("&"), rep(list("+"), 4), list("diff"), list("&"), \(x, y)length(x)==length(y))
ops2 <- c(rep(list("=="), 7), \(x, y)length(x)==length(y))
ops <- c(ops1, ops2)

for(i in seq_along(funs)) {
  for(xBC in c(TRUE, FALSE)) {
    for(yBC in c(TRUE, FALSE)) {
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      broadcaster(x) <- xBC
      broadcaster(y) <- yBC
      
      out <- funs[[i]](x, y, ops[[i]])
      
      expect_equal(
        broadcaster(out),
        broadcaster(x) || broadcaster(y)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}

