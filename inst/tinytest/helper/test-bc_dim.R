
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
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


# main ====
nres <- 10 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
x.data <- sample(c(-10:10, NA), 100, TRUE)
y.data <- sample(c(-10:10, NA), 100, TRUE)

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in sample(1:8, 3L)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data[1:x.len], dim = x.dim)
      y <- array(y.data[1:y.len], dim = y.dim)
      
      for(iSimplify in c(0, 1, 2)) {
        if(length(x.dim) == 1 && iSimplify %in% c(0, 2)) {
          dim(x) <- NULL
        }
        if(length(y.dim) == 1 && iSimplify %in% c(1, 2)) {
          dim(y) <- NULL
        }
      }
      
      res <- bc.b(x, y, "&")
      expected[[i]] <- dim(res)
      if(is.null(dim(res))) expected[[i]] <- length(res)
      
      out[[i]] <- bc_dim(x, y)
      
      expect_equal(
        expected[[i]], out[[i]]
      ) |> errorfun()
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)

