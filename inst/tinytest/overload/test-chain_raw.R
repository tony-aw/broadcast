
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


# bit-wise and ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "&"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- as.raw(sample(0:255, 100))
  y.data <- as.raw(sample(0:255, 100))
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in sample(1:8, 3L)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)

        x <- array(x.data[1:x.len], dim = x.dim)
        y <- array(y.data[1:y.len], dim = y.dim)
        
        # PREPARE FOR TEST
        tdim <- bc_dim(x, y)
        # print(x)
        # print(y)
        # print(tdim)
        # cat("\n")
        
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected[[i]] <- as_raw(drop(x)) & as_raw(drop(y))
          attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
          out[[i]] <- bc_chain(~ x & y)
        }
        else if(length(y) == 1L && length(x) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected[[i]] <- as.raw(x) & as.raw(y)
          out[[i]] <- bc_chain(~ x & y)
        }
        else if(length(x) == 1L && length(y) > 1L) {
          # CASE 3: x is scalar, y is not
          expected[[i]] <- as.raw(x) & rep_dim(as_raw(y), tdim)
          out[[i]] <- bc_chain(~ x & y)
        }
        else if(length(y) == 1L && length(x) > 1L) {
          # CASE 4: y is scalar, x is not
          expected[[i]] <- rep_dim(as_raw(x), tdim) & as.raw(y)
          out[[i]] <- bc_chain(~ x & y)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected[[i]] <- rep_dim(as_raw(x), tdim) & rep_dim(as_raw(y), tdim)
          out[[i]] <- bc_chain(~ x & y)
        }
        # END CASES
        
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


# bir-wise or ====
nres <- 10 * 5 * 5 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "|"

i <- 1L
for(iSample in 1:10) { # re-do tests with different random configurations
  x.data <- as.raw(sample(0:255, 100))
  y.data <- as.raw(sample(0:255, 100))
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in sample(1:8, 3L)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(x.data[1:x.len], dim = x.dim)
      y <- array(y.data[1:y.len], dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected[[i]] <- as_raw(drop(x)) | as_raw(drop(y))
        attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
        out[[i]] <- bc_chain(~ x | y)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected[[i]] <- as.raw(x) | as.raw(y)
        out[[i]] <- bc_chain(~ x | y)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected[[i]] <- as.raw(x) | rep_dim(as_raw(y), tdim)
        out[[i]] <- bc_chain(~ x | y)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected[[i]] <- rep_dim(as_raw(x), tdim) | as.raw(y)
        out[[i]] <- bc_chain(~ x | y)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected[[i]] <- rep_dim(as_raw(x), tdim) | rep_dim(as_raw(y), tdim)
        out[[i]] <- bc_chain(~ x | y)
      }
      # END CASES
      
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
