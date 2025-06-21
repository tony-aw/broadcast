
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
.return_missing <- broadcast:::.return_missing



# logical ====
gen <- function(n) sample(c(TRUE, FALSE, NA), n, TRUE)
i <- 1L
op <- function(x, y) {
  return(x == y)
}
v <- "logical"
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- as.logical(out)
  dim(out) <- bc_dim(x, y)
  return(out)
}


for(iSample in 1:5) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(gen(x.len), dim = x.dim)
      y <- array(gen(y.len), dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected <- basefun((drop(x)), (drop(y)))
        attributes(expected) <- NULL # must be a vector if tdim == NULL
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected <- basefun((x), (y))
        out <- bcapply(x, y, op, v)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected <- basefun((x), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected <- basefun(rep_dim((x), tdim), (y))
        out <- bcapply(x, y, op, v)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected <- basefun(rep_dim((x), tdim), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      # END CASES
      
      # ensure correct dimensions:
      dim(expected) <- tdim
      
      expect_equal(
        expected, out
      ) |> errorfun()
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:



# integer (32bit) ====
gen <- function(n) sample(c(-10:10, NA), n, TRUE)
i <- 1L
op <- function(x, y) {
  return(x + y)
}
v <- "integer"
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- as.integer(out)
  dim(out) <- bc_dim(x, y)
  return(out)
}


for(iSample in 1:5) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(gen(x.len), dim = x.dim)
      y <- array(gen(y.len), dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected <- basefun((drop(x)), (drop(y)))
        attributes(expected) <- NULL # must be a vector if tdim == NULL
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected <- basefun((x), (y))
        out <- bcapply(x, y, op, v)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected <- basefun((x), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected <- basefun(rep_dim((x), tdim), (y))
        out <- bcapply(x, y, op, v)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected <- basefun(rep_dim((x), tdim), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      # END CASES
      
      # ensure correct dimensions:
      dim(expected) <- tdim
      
      expect_equal(
        expected, out
      ) |> errorfun()
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:



# double (64bit) ====
gen <- function(n) sample(c(rnorm(21), NA, NaN, Inf, -Inf), n, TRUE)
i <- 1L
op <- function(x, y) {
  return(x + y)
}
v <- "double"
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- as.double(out)
  dim(out) <- bc_dim(x, y)
  return(out)
}


for(iSample in 1:5) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(gen(x.len), dim = x.dim)
      y <- array(gen(y.len), dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected <- basefun((drop(x)), (drop(y)))
        attributes(expected) <- NULL # must be a vector if tdim == NULL
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected <- basefun((x), (y))
        out <- bcapply(x, y, op, v)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected <- basefun((x), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected <- basefun(rep_dim((x), tdim), (y))
        out <- bcapply(x, y, op, v)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected <- basefun(rep_dim((x), tdim), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      # END CASES
      
      # ensure correct dimensions:
      dim(expected) <- tdim
      
      expect_equal(
        expected, out
      ) |> errorfun()
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:



# complex ====
gen <- function(n) sample(c(rnorm(21), NA, NaN, Inf, -Inf), n, TRUE) + sample(c(rnorm(21), NA, NaN, Inf, -Inf), n, TRUE) * -1i
i <- 1L
op <- function(x, y) {
  return(x + y)
}
v <- "complex"
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- as.complex(out)
  dim(out) <- bc_dim(x, y)
  return(out)
}


for(iSample in 1:5) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)
      
      x <- array(gen(x.len), dim = x.dim)
      y <- array(gen(y.len), dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected <- basefun((drop(x)), (drop(y)))
        attributes(expected) <- NULL # must be a vector if tdim == NULL
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected <- basefun((x), (y))
        out <- bcapply(x, y, op, v)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected <- basefun((x), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected <- basefun(rep_dim((x), tdim), (y))
        out <- bcapply(x, y, op, v)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected <- basefun(rep_dim((x), tdim), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      # END CASES
      
      # ensure correct dimensions:
      dim(expected) <- tdim
      
      expect_equal(
        expected, out
      ) |> errorfun()
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:



# string ====
gen <- function(n) sample(c(letters, month.abb), n, TRUE)
i <- 1L
op <- function(x, y) {
  paste(x, y, collapse = "", sep = "")
}
v <- "character"
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- as.character(out)
  dim(out) <- bc_dim(x, y)
  return(out)
}


for(iSample in 1:5) { # re-do tests with different random configurations
  for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
      y.dim <- test_make_dims(iDimY)
      y.len <- prod(y.dim)

      x <- array(gen(x.len), dim = x.dim)
      y <- array(gen(y.len), dim = y.dim)
      
      # PREPARE FOR TEST
      tdim <- bc_dim(x, y)
      # print(x)
      # print(y)
      # print(tdim)
      # cat("\n")
      
      
      # DO TESTS BY CASE:
      if(is.null(tdim)) {
        # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
        expected <- basefun((drop(x)), (drop(y)))
        attributes(expected) <- NULL # must be a vector if tdim == NULL
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected <- basefun((x), (y))
        out <- bcapply(x, y, op, v)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected <- basefun((x), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected <- basefun(rep_dim((x), tdim), (y))
        out <- bcapply(x, y, op, v)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected <- basefun(rep_dim((x), tdim), rep_dim((y), tdim))
        out <- bcapply(x, y, op, v)
      }
      # END CASES
      
      # ensure correct dimensions:
      dim(expected) <- tdim
      
      expect_equal(
        expected, out
      ) |> errorfun()
      
      i <- i + 1L
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:


