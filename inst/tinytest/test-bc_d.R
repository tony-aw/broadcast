
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


# plus ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "+"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)

      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- as_dbl(drop(x)) + as_dbl(drop(y))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- as.double(x) + as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- as.double(x) + array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- array_recycle(as_dbl(x), tdim) + as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- array_recycle(as_dbl(x), tdim) + array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
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
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# minus ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "-"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- as_dbl(drop(x)) - as_dbl(drop(y))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- as.double(x) - as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- as.double(x) - array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- array_recycle(as_dbl(x), tdim) - as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- array_recycle(as_dbl(x), tdim) - array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
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
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# multiply ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "*"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- as_dbl(drop(x)) * as_dbl(drop(y))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- as.double(x) * as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- as.double(x) * array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- array_recycle(as_dbl(x), tdim) * as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- array_recycle(as_dbl(x), tdim) * array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
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
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# divide ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "/"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- as_dbl(drop(x)) / as_dbl(drop(y))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- as.double(x) / as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- as.double(x) / array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- array_recycle(as_dbl(x), tdim) / as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- array_recycle(as_dbl(x), tdim) / array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
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
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)




# power ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "^"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- as_dbl(drop(x)) ^ as_dbl(drop(y))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- as.double(x) ^ as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- as.double(x) ^ array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- array_recycle(as_dbl(x), tdim) ^ as.double(y)
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- array_recycle(as_dbl(x), tdim) ^ array_recycle(as_dbl(y), tdim)
            out[[i]] <- bc.d(x, y, op)
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
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



# pmin ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "pmin"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- pmin(as_dbl(drop(x)), as_dbl(drop(y)))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- pmin(as.double(x), as.double(y))
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- pmin(as.double(x), array_recycle(as_dbl(y), tdim))
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- pmin(array_recycle(as_dbl(x), tdim), as.double(y))
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- pmin(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
            out[[i]] <- bc.d(x, y, op)
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
          
          dim(expected[[i]]) <- tdim # pmin/pmax don't always keep dimensions, so have to re-assign them here
          
          i <- i + 1L
        }
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)




# pmax ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "pmax"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(1:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          # PREPARE FOR TEST
          tdim <- bc_pred_dim(x, y)
          # print(x)
          # print(y)
          # print(tdim)
          # cat("\n")
          
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- pmax(as_dbl(drop(x)), as_dbl(drop(y)))
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- pmax(as.double(x), as.double(y))
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(x) == 1L && length(y) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- pmax(as.double(x), array_recycle(as_dbl(y), tdim))
            out[[i]] <- bc.d(x, y, op)
          }
          else if(length(y) == 1L && length(x) > 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- pmax(array_recycle(as_dbl(x), tdim), as.double(y))
            out[[i]] <- bc.d(x, y, op)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- pmax(array_recycle(as_dbl(x), tdim), array_recycle(as_dbl(y), tdim))
            out[[i]] <- bc.d(x, y, op)
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
          
          dim(expected[[i]]) <- tdim # pmin/pmax don't always keep dimensions, so have to re-assign them here
          
          i <- i + 1L
        }
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)



