# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

funlist <- list(
  as_bool,
  as_int,
  as_dbl,
  as_cplx,
  as_raw
)

valuelist <- list(
  c(TRUE, FALSE, NA),
  1:10,
  rnorm(10),
  rnorm(10) * -1i,
  as.raw(0:255)
)

for(i in seq_along(valuelist)) {
  x <- rep(list(valuelist[[i]]), 4L)
  dim(x) <- c(2,2)
  expect_error(
    funlist[[i]](x)
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

