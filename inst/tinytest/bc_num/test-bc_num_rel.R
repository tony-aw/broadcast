
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

prec <- sqrt(.Machine$double.eps)


# Note:
# These are primarily dimensional consistency tests.
# For tests about whether the rel ops are actually accurate,
# see test-relop_precision.R

# basic tests ====

x <- as.array(1:100)
y <- as.array(sample(1:100))



# equals, numeric x ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d=="

i <- 1L
x.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- abs(x - y) < prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)

      x <- array(x.data, dim = x.dim)
      for(iDataY in 1:length(y.data)) { # different data types for y
        y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# equals, numeric y ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d=="

i <- 1L
y.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- abs(x - y) < prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      y <- array(y.data, dim = y.dim)
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


# not-equals, numeric x ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d!="

i <- 1L
x.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- abs(x - y) >= prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data, dim = x.dim)
      for(iDataY in 1:length(y.data)) { # different data types for y
        y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# not-equals, numeric y ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d!="

i <- 1L
y.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- abs(x - y) >= prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      y <- array(y.data, dim = y.dim)
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# smaller than, numeric x ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d<"

i <- 1L
x.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) <= -prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data, dim = x.dim)
      for(iDataY in 1:length(y.data)) { # different data types for y
        y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# smaller than, numeric y ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d<"

i <- 1L
y.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) <= -prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      y <- array(y.data, dim = y.dim)
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)





# greater than, numeric x ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d>"

i <- 1L
x.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) >= prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data, dim = x.dim)
      for(iDataY in 1:length(y.data)) { # different data types for y
        y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# greater than, numeric y ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d>"

i <- 1L
y.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) >= prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      y <- array(y.data, dim = y.dim)
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)




# se, numeric x ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d<="

i <- 1L
x.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) < prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data, dim = x.dim)
      for(iDataY in 1:length(y.data)) { # different data types for y
        y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# se, numeric y ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d<="

i <- 1L
y.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) < prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      y <- array(y.data, dim = y.dim)
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)





# ge, numeric x ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d>="

i <- 1L
x.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) > -prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data, dim = x.dim)
      for(iDataY in 1:length(y.data)) { # different data types for y
        y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# ge, numeric y ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "d>="

i <- 1L
y.data <- sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
basefun <- function(x, y) {
  out <- (x - y) > -prec
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    rnorm(100) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      y <- array(y.data, dim = y.dim)
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(as_dbl(drop(x)), as_dbl(drop(y)))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(as.double(x), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(as.double(x), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), as.double(y))
          out[[i]] <- bc.num(x, y, op)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
          out[[i]] <- bc.num(x, y, op)
        }
        # END CASES
        
        # R is sometimes inconsistent whether it returns NA or NaN
        # for example: NaN + NaN = NA, but NaN - NaN = NaN
        # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
        # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
        ind.NaN <- is.nan(expected[[i]])
        expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
        ind.NaN <- is.nan(out[[i]])
        out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
        
        # ensure correct dimensions:
        dim(expected[[i]]) <- tdim
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)




