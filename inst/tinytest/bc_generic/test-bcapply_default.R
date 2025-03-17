
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
gen <- function(n) sample(list(letters, month.abb, 1:10), n, TRUE)


# recursive ====

i <- 1L

op <- function(x, y) {
  c(length(x) == length(y), typeof(x) == typeof(y))
}
basefun <- function(x, y) {
  # using for-loop, because mapply really does not function properly here
  out <- mapply(op, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
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
        out <- bcapply(x, y, op)
      }
      else if(length(y) == 1L && length(x) == 1L) {
        # CASE 2: x and y are both scalar arrays
        expected <- basefun((x), (y))
        out <- bcapply(x, y, op)
      }
      else if(length(x) == 1L && length(y) > 1L) {
        # CASE 3: x is scalar, y is not
        expected <- basefun((x), rep_dim((y), tdim))
        out <- bcapply(x, y, op)
      }
      else if(length(y) == 1L && length(x) > 1L) {
        # CASE 4: y is scalar, x is not
        expected <- basefun(rep_dim((x), tdim), (y))
        out <- bcapply(x, y, op)
      }
      else {
        # CASE 5: x and y are both non-reducible arrays
        expected <- basefun(rep_dim((x), tdim), rep_dim((y), tdim))
        out <- bcapply(x, y, op)
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


# errors ====
x <- 1:10
y <- array(1:10, c(1, 10))
expect_error(
  bcapply(x, y, ~ hello),
  pattern = "`f` must be a function",
  fixed = TRUE
)
f <- \(x) x^2
expect_error(
  bcapply(x, y, f),
  pattern = "`f` must be a function that takes in exactly 2 arguments",
  fixed = TRUE
)
enumerate <- enumerate + 2L

