# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_address <- broadcast:::.rcpp_address

funlist <- list(
  as_lgl,
  as_int,
  as_dbl,
  as_cplx,
  as_raw,
  as_str
)

valuelist <- list(
  c(TRUE, FALSE, NA),
  1:10,
  rnorm(10),
  rnorm(10) * -1i,
  as.raw(0:255),
  sample(letters)
)

for(i in seq_along(valuelist)) {
  x <- as.array(valuelist[[i]])
  addressX <- .rcpp_address(x)
  y <- funlist[[i]](x)
  addressY <- .rcpp_address(y)
  
  expect_equal(
    x, y
  ) |> errorfun()
  
  expect_false(addressX == addressY) |> errorfun()
  
  enumerate <- enumerate + 2L
}

