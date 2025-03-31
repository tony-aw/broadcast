
# set-up ====

enumerate <- 0 # to count number of tests in loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  rnorm(100),
  sample(c(NA, NaN, -Inf, Inf, 0), 100, TRUE),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  as.complex(c(1:99, NA)),
  as.raw(0:99),
  rep(NA, 100)
)
indices <- list(
  2, 10:5, 1:100
)


# 32 bit ====
myset <- broadcast:::.rcpp_set_vind_32


for(iD in 1:length(x.data)) {
  for(iIndices in 1:length(indices)) {
    temp.ind <- as.integer(indices[[iIndices]])
    
    x <- x.data[[iD]]
    x2 <- x
    x2[ temp.ind ] <- rev(x2[ temp.ind ])
    rp <- rev(x[ temp.ind ])
    myset(x, temp.ind - 1L, rp = rp)
    invisible(x) # waking up R
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 1
    
  }
}



# 64 bit ====
myset <- broadcast:::.rcpp_set_vind_64


for(iD in 1:length(x.data)) {
  for(iIndices in 1:length(indices)) {
    temp.ind <- as.double(indices[[iIndices]])
    
    x <- x.data[[iD]]
    x2 <- x
    x2[ temp.ind ] <- rev(x2[ temp.ind ])
    rp <- rev(x[ temp.ind ])
    myset(x, temp.ind - 1.0, rp = rp)
    invisible(x) # waking up R
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 1
    
  }
}

