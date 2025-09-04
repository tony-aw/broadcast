# test binary attributes
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(0:9))
)


for(i in seq_along(datagens)) {
  for(xBC in c(TRUE, FALSE)) {
    for(yBC in c(TRUE, FALSE)) {
      
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      broadcaster(x) <- xBC
      broadcaster(y) <- yBC
      
      out <- bc.rel(x, y, "==")
      expect_equal(
        broadcaster(out),
        broadcaster(x) || broadcaster(y)
      ) |> errorfun()
      
      
      enumerate <- enumerate + 1L
      
    }
  }
}

