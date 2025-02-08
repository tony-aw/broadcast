
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 3:
  out <- lapply(1:n, \(n)sample(c(1, 3), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}

datagens <- list(
  # \() as.raw(sample(1:10)), # ifelse() cannot handle raw, apparently
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)



along <- 1L


for(iSample in 1:5) { # re-do tests with different random configurations
  for(iData in 1:length(datagens)) {
    x.data <- datagens[[iData]]()
    y.data <- datagens[[iData]]()
    for(iDimX in c(1, 2, 5, 8)) { # dim(x)[along]
      for(iDimY in c(1, 2, 5, 8)) { # dim(y)[along]
        for(nDim in 1:3) { # number of dimensions in total, shared by both x and y
          x.dim <- y.dim <- test_make_dims(nDim)
          if(length(x.dim) >= along) {
            x.dim[along] <- iDimX
            y.dim[along] <- iDimY
          }
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          for(nInput in 1:4) { # length of input list
            input <- list(x, y, x, y)
            input <- input[1:nInput]
            
            expected <- abind::abind(input, along = 1L)
            out <- bind_array(input, 1L)
            
            if(is.list())
            
            expect_equivalent(
              expected, out
            ) |> errorfun()
            
          }
        }
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:



