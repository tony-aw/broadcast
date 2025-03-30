
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 5:
  out <- lapply(1:n, \(n)sample(c(1, 5), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}
.return_missing <- broadcast:::.return_missing


# and ====
nres <- 10 * 5 * 5 # number of tests performed here
expected <- out <- vector("list", nres)
x.data <- sample(c(-10:10, NA), 100, TRUE)
y.data <- sample(c(-10:10, NA), 100, TRUE)

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      x <- array(x.data[1:x.len], dim = x.dim)
      y <- array(y.data[1:y.len], dim = y.dim)
      
      expected[[i]] <- dim(bc.b(x, y, "&"))
      out[[i]] <- bc_dim(x, y)
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)

