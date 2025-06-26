
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

gen <- function() {
  sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE)
}

accept_test <- function(x, y) {
  check1 <- broadcast:::.is_numeric_like(x) || broadcast:::.is_numeric_like(y)
  check2 <- is.raw(x) || is.raw(y)
  if(check1 && check2) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}




# equals ====
nres <- 5 * 6 * 6 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
basefun <- function(x, y) {
  out <- x == y

  return(out)
}
testfun <- function(x, y) {
  ab(x) == ab(y)
}


i <- 1L
for(iSample in 1:5) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    gen(), # double,
    gen() + gen() * -1i, # complex,
    sample(sample(letters, 100, TRUE)), # character
    sample(as.raw(0:255), 100) # raw
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    gen(), # double,
    gen() + gen() * -1i, # complex,
    sample(sample(letters, 100, TRUE)), # character
    sample(as.raw(0:255), 100) # raw
  )
  
  
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in sample(1:8, 3L)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          if(accept_test(x, y)) {
            
            
            # PREPARE FOR TEST
            tdim <- bc_dim(x, y)
            # print(x)
            # print(y)
            # print(tdim)
            # # cat("\n")
            # 
            # cat("dim(x) = ", dim(x), "\n")
            # cat("dim(y) = ", dim(y), "\n")
            
            # DO TESTS BY CASE:
            if(is.null(tdim)) {
              # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
              expected[[i]] <- basefun(drop(x), drop(y))
              
              # attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
              out[[i]] <- testfun(x, y)
            }
            else if(length(y) == 1L && length(x) == 1L) {
              # CASE 2: x and y are both scalar arrays
              expected[[i]] <- basefun(as.vector(x), as.vector(y))
              out[[i]] <- testfun(x, y)
            }
            else if(length(x) == 1L && length(y) > 1L) {
              # CASE 3: x is scalar, y is not
              expected[[i]] <- basefun(as.vector(x), rep_dim(y, tdim))
              out[[i]] <- testfun(x, y)
            }
            else if(length(y) == 1L && length(x) > 1L) {
              # CASE 4: y is scalar, x is not
              expected[[i]] <- basefun(rep_dim(x, tdim), as.vector(y))
              out[[i]] <- testfun(x, y)
            }
            else {
              # CASE 5: x and y are both non-reducible arrays
              expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
              out[[i]] <- testfun(x, y)
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
            
            dim(expected[[i]]) <- tdim
            
            out[[i]] <- unclass(out[[i]]) # because broadcaster attribute is preserved
            
            
            i <- i + 1L
          }
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



# unequals ====
nres <- 5 * 6 * 6 * 3 * 3 # number of tests performed here
expected <- out <- vector("list", nres)
basefun <- function(x, y) {
  out <- x != y
  
  return(out)
}
testfun <- function(x, y) {
  ab(x) != ab(y)
}


i <- 1L
for(iSample in 1:5) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    gen(), # double,
    gen() + gen() * -1i, # complex,
    sample(sample(letters, 100, TRUE)), # character
    sample(as.raw(0:255), 100) # raw
  )
  y.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    gen(), # double,
    gen() + gen() * -1i, # complex,
    sample(sample(letters, 100, TRUE)), # character
    sample(as.raw(0:255), 100) # raw
  )
  
  
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in sample(1:8, 3L)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      for(iDataX in 1:length(x.data)) { # different data types for x
        x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
        for(iDataY in 1:length(y.data)) { # different data types for y
          y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
          
          if(accept_test(x, y)) {
            
            
            # PREPARE FOR TEST
            tdim <- bc_dim(x, y)
            # print(x)
            # print(y)
            # print(tdim)
            # # cat("\n")
            # 
            # cat("dim(x) = ", dim(x), "\n")
            # cat("dim(y) = ", dim(y), "\n")
            
            # DO TESTS BY CASE:
            if(is.null(tdim)) {
              # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
              expected[[i]] <- basefun(drop(x), drop(y))
              
              # attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
              out[[i]] <- testfun(x, y)
            }
            else if(length(y) == 1L && length(x) == 1L) {
              # CASE 2: x and y are both scalar arrays
              expected[[i]] <- basefun(as.vector(x), as.vector(y))
              out[[i]] <- testfun(x, y)
            }
            else if(length(x) == 1L && length(y) > 1L) {
              # CASE 3: x is scalar, y is not
              expected[[i]] <- basefun(as.vector(x), rep_dim(y, tdim))
              out[[i]] <- testfun(x, y)
            }
            else if(length(y) == 1L && length(x) > 1L) {
              # CASE 4: y is scalar, x is not
              expected[[i]] <- basefun(rep_dim(x, tdim), as.vector(y))
              out[[i]] <- testfun(x, y)
            }
            else {
              # CASE 5: x and y are both non-reducible arrays
              expected[[i]] <- basefun(rep_dim(x, tdim), rep_dim(y, tdim))
              out[[i]] <- testfun(x, y)
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
            
            dim(expected[[i]]) <- tdim
            
            out[[i]] <- unclass(out[[i]]) # because broadcaster attribute is preserved
            
            i <- i + 1L
          }
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


