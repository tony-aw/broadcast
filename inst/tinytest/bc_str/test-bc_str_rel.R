
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
.return_NA <- broadcast:::.return_NA

prec <- sqrt(.Machine$double.eps)


# basic tests ====

x <- as.array(sample(letters))
y <- as.array(sample(letters))
expect_equal(
  bc.str(x, y, "=="),
  as.vector(x == y)
)
expect_equal(
  bc.str(x, y, "!="),
  as.vector(x != y)
)

x <- as.array(sample(letters))
y <- as.array(sample(letters))
expect_equal(
  bc.str(x, y, "=="),
  as.vector(x == y)
)
expect_equal(
  bc.str(x, y, "!="),
  as.vector(x != y)
)

enumerate <- enumerate + 4L



# equals ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "=="

i <- 1L
x.data <- sample(letters)
y.data <- sample(letters)
basefun <- function(x, y) {
  out <- x == y
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)

      x <- array(x.data, dim = x.dim)
      y <- array(y.data, dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected[[i]] <- basefun(as_chr(drop(x)), as_chr(drop(y)))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc.str(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- basefun(as.character(x), as.character(y))
        out[[i]] <- bc.str(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- basefun(as.character(x), array_recycle(as_chr(y), tdim))
        out[[i]] <- bc.str(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- basefun(array_recycle(as_chr(x), tdim), as.character(y))
        out[[i]] <- bc.str(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- basefun(array_recycle(as_chr(x), tdim), array_recycle(as_chr(y), tdim))
        out[[i]] <- bc.str(x, y, op)
      }
      # END CASES
      
      # R is sometimes inconsistent whether it returns NA or NaN
      # for example: NaN + NaN = NA, but NaN - NaN = NaN
      # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
      # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
      ind.NaN <- is.nan(expected[[i]])
      expected[[i]][ind.NaN] <- .return_NA(expected[[i]][ind.NaN])
      ind.NaN <- is.nan(out[[i]])
      out[[i]][ind.NaN] <- .return_NA(out[[i]][ind.NaN])
      
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


# not equals ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "!="

i <- 1L
x.data <- sample(letters)
y.data <- sample(letters)
basefun <- function(x, y) {
  out <- x != y
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data, dim = x.dim)
      y <- array(y.data, dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected[[i]] <- basefun(as_chr(drop(x)), as_chr(drop(y)))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc.str(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- basefun(as.character(x), as.character(y))
        out[[i]] <- bc.str(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- basefun(as.character(x), array_recycle(as_chr(y), tdim))
        out[[i]] <- bc.str(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- basefun(array_recycle(as_chr(x), tdim), as.character(y))
        out[[i]] <- bc.str(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- basefun(array_recycle(as_chr(x), tdim), array_recycle(as_chr(y), tdim))
        out[[i]] <- bc.str(x, y, op)
      }
      # END CASES
      
      # R is sometimes inconsistent whether it returns NA or NaN
      # for example: NaN + NaN = NA, but NaN - NaN = NaN
      # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
      # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
      ind.NaN <- is.nan(expected[[i]])
      expected[[i]][ind.NaN] <- .return_NA(expected[[i]][ind.NaN])
      ind.NaN <- is.nan(out[[i]])
      out[[i]][ind.NaN] <- .return_NA(out[[i]][ind.NaN])
      
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)

