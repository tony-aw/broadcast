
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

ab <- broadcast:::.as.broadcaster

gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))




# plus - cplx by other ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)

i <- 1L
basefun <- function(x, y) {
  out <- x + y
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- gen() + gen() * -1i
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iData in seq_along(y.data)) {
    for(iDimX in sample(1:8, 3L)) { # different dimensions for x
      x.dim <- test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, 3L)) { # different dimensions for y
        y.dim <- test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        x <- array(x.data, dim = x.dim)
        y <- array(y.data[[iData]], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(drop(x), drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- ab(x) + ab(y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(drop(x), drop(y))
          out[[i]] <- ab(x) + ab(y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(drop(x), rep_dim(y, tdim))
          out[[i]] <- ab(x) + ab(y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(rep_dim(x, tdim), drop(y))
          out[[i]] <- ab(x) + ab(y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
          out[[i]] <- ab(x) + ab(y)
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
        
        out[[i]] <- unclass(out[[i]])
        
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



# plus - other by cplx ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "+"

i <- 1L
basefun <- function(x, y) {
  out <- x + y
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- gen() + gen() * -1i
  for(iData in seq_along(x.data)) {
    for(iDimX in sample(1:8, 3L)) { # different dimensions for x
      x.dim <- test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, 3L)) { # different dimensions for y
        y.dim <- test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        x <- array(x.data[[iData]], dim = x.dim)
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
          expected[[i]] <- basefun(drop(x), drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- ab(x) + ab(y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(drop(x), drop(y))
          out[[i]] <- ab(x) + ab(y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(drop(x), rep_dim(y, tdim))
          out[[i]] <- ab(x) + ab(y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(rep_dim(x, tdim), drop(y))
          out[[i]] <- ab(x) + ab(y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
          out[[i]] <- ab(x) + ab(y)
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
        
        out[[i]] <- unclass(out[[i]])
        
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


# min - cplx by other ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "+"

i <- 1L
basefun <- function(x, y) {
  out <- x - y
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- gen() + gen() * -1i
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iData in seq_along(y.data)) {
    for(iDimX in sample(1:8, 3L)) { # different dimensions for x
      x.dim <- test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, 3L)) { # different dimensions for y
        y.dim <- test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        x <- array(x.data, dim = x.dim)
        y <- array(y.data[[iData]], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(drop(x), drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- ab(x) - ab(y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(drop(x), drop(y))
          out[[i]] <- ab(x) - ab(y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(drop(x), rep_dim(y, tdim))
          out[[i]] <- ab(x) - ab(y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(rep_dim(x, tdim), drop(y))
          out[[i]] <- ab(x) - ab(y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
          out[[i]] <- ab(x) - ab(y)
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
        
        out[[i]] <- unclass(out[[i]])
        
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



# min - other by cplx ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "+"

i <- 1L
basefun <- function(x, y) {
  out <- x - y
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- gen() + gen() * -1i
  for(iData in seq_along(x.data)) {
    for(iDimX in sample(1:8, 3L)) { # different dimensions for x
      x.dim <- test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, 3L)) { # different dimensions for y
        y.dim <- test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        x <- array(x.data[[iData]], dim = x.dim)
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
          expected[[i]] <- basefun(drop(x), drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- ab(x) - ab(y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(drop(x), drop(y))
          out[[i]] <- ab(x) - ab(y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(drop(x), rep_dim(y, tdim))
          out[[i]] <- ab(x) - ab(y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(rep_dim(x, tdim), drop(y))
          out[[i]] <- ab(x) - ab(y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
          out[[i]] <- ab(x) - ab(y)
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
        
        out[[i]] <- unclass(out[[i]])
        
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



# mult - cplx by other ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)

i <- 1L
basefun <- function(x, y) {
  out <- x * y
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- gen() + gen() * -1i
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  for(iData in seq_along(y.data)) {
    for(iDimX in sample(1:8, 3L)) { # different dimensions for x
      x.dim <- test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, 3L)) { # different dimensions for y
        y.dim <- test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        x <- array(x.data, dim = x.dim)
        y <- array(y.data[[iData]], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- basefun(drop(x), drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- ab(x) * ab(y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(drop(x), drop(y))
          out[[i]] <- ab(x) * ab(y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(drop(x), rep_dim(y, tdim))
          out[[i]] <- ab(x) * ab(y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(rep_dim(x, tdim), drop(y))
          out[[i]] <- ab(x) * ab(y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
          out[[i]] <- ab(x) * ab(y)
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
        
        out[[i]] <- unclass(out[[i]])
        
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



# mult - other by cplx ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)

i <- 1L
basefun <- function(x, y) {
  out <- x * y
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE) # double
  )
  y.data <- gen() + gen() * -1i
  for(iData in seq_along(x.data)) {
    for(iDimX in sample(1:8, 3L)) { # different dimensions for x
      x.dim <- test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, 3L)) { # different dimensions for y
        y.dim <- test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        x <- array(x.data[[iData]], dim = x.dim)
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
          expected[[i]] <- basefun(drop(x), drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- ab(x) * ab(y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- basefun(drop(x), drop(y))
          out[[i]] <- ab(x) * ab(y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- basefun(drop(x), rep_dim(y, tdim))
          out[[i]] <- ab(x) * ab(y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- basefun(rep_dim(x, tdim), drop(y))
          out[[i]] <- ab(x) * ab(y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
          out[[i]] <- ab(x) * ab(y)
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
        
        out[[i]] <- unclass(out[[i]])
        
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



# division tests: coming later...
