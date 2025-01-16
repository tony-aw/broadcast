
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

gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))


# basic tests ====

x <- as.array(gen() + gen() * -1i)
y <- as.array(gen() + gen() * -1i)
expect_equal(
  bc.cplx(x, y, "+") |> as.vector(),
  as.vector(x + y)
)

x <- as.array(gen() + gen() * -1i)
y <- as.array(gen() + gen() * -1i)
expect_equal(
  bc.cplx(x, y, "+") |> as.vector(),
  as.vector(x + y)
)
enumerate <- enumerate + 2L



# plus ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "+"

i <- 1L
x.data <- gen() + gen() * -1i
y.data <- gen() + gen() * -1i
basefun <- function(x, y) {
  out <- x + y
  dim(out) <- bc_dim(x, y)
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
        expected[[i]] <- basefun(as_cplx(drop(x)), as_cplx(drop(y)))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- basefun(as.complex(x), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- basefun(as.complex(x), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
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
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


# min ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "-"

i <- 1L
x.data <- gen() + gen() * -1i
y.data <- gen() + gen() * -1i
basefun <- function(x, y) {
  out <- x - y
  out[is.na(x)|is.na(y)] <- NA
  dim(out) <- bc_dim(x, y)
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
        expected[[i]] <- basefun(as_cplx(drop(x)), as_cplx(drop(y)))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- basefun(as.complex(x), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- basefun(as.complex(x), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
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
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


# multiply ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "*"

i <- 1L
x.data <- gen() + gen() * -1i
y.data <- gen() + gen() * -1i
basefun <- function(x, y) {
  out <- x * y
  out[is.na(x)|is.na(y)] <- NA
  dim(out) <- bc_dim(x, y)
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
        expected[[i]] <- basefun(as_cplx(drop(x)), as_cplx(drop(y)))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- basefun(as.complex(x), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- basefun(as.complex(x), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
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
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


# div ====
nres <- 10 * 5 * 5 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "/"

i <- 1L
x.data <- gen() + gen() * -1i
y.data <- gen() + gen() * -1i
basefun <- function(x, y) {
  out <- x / y
  out[is.na(x)|is.na(y)] <- NA
  dim(out) <- bc_dim(x, y)
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
        expected[[i]] <- basefun(as_cplx(drop(x)), as_cplx(drop(y)))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- basefun(as.complex(x), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- basefun(as.complex(x), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), as.complex(y))
        out[[i]] <- bc.cplx(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- basefun(array_recycle(as_cplx(x), tdim), array_recycle(as_cplx(y), tdim))
        out[[i]] <- bc.cplx(x, y, op)
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
enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


