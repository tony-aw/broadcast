# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() list(letters, month.abb)
)


counter <- 1L
for(i in seq_along(datagens)) {
  for(xLen in c(10, 0)) {
    for(yLen in c(0, 10)) {
      
      # prep data:
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      if(xLen == 0L) {
        x <- x[0L]
      }
      else if(yLen == 0L) {
        y <- y[0L]
      }
      else {
        x <- x[0L]
        y <- y[0L]
      }
      
      cond <- logical(0L)
      expect_equal(
        bc_ifelse(cond, x, y),
        vector(typeof(x), 0L)
      ) |> errorfun()
      
      expect_equal(
        bcapply(x, y, \(x, y)paste0(x, y), typeof(x)),
        vector(typeof(x), 0L)
      ) |> errorfun()
      
      
      enumerate <- enumerate + 2L
      
    }
  }
}

