
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# overflow ====
expect_equal(
  bc.i(2^54, 1, "+") |> drop(),
  Inf
)

expect_equal(
  bc.i(-2^54, 1, "-") |> drop(),
  -Inf
)
enumerate <- enumerate + 2L


# gcd simple ====
gcd <- function(x,y) { # straight-forward definition of gcd, for testing
  r <- x %% y
  return(ifelse(r, gcd(y, r), y))
}

expect_equal(
  bc.i(10, 8, "gcd") |> drop(),
  2
)
expect_equal(
  bc.i(-10, 8, "gcd") |> drop(),
  2
)
expect_equal(
  bc.i(10, -8, "gcd") |> drop(),
  2
)

x <- sample(1:100, 10)
y <- sample(1:100, 10)
expect_equal(
  bc.i(x, y, "gcd"),
  gcd(x, y) |> as.array()
)
enumerate <- enumerate + 4L



# gcd with zero ====
expect_equal(
  bc.i(0, 0, "gcd") |> drop(),
  NA_real_
)
samp <- sample(1:100, 10) * c(-1, 1)
for(i in samp) {
  expect_equal(
    bc.i(i, 0, "gcd") |> drop(),
    i
  ) |> errorfun()
  expect_equal(
    bc.i(0, i, "gcd") |> drop(),
    i
  ) |> errorfun()
  enumerate <- enumerate + 2L
}


# gcd with NAs and Infs ====

x.data <- list(
  NA,
  NA_integer_,
  c(Inf, -Inf, NA, NaN)
)
y.data <- list(
  NA,
  NA_integer_,
  c(Inf, -Inf, NA, NaN)
)
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- array(x.data[[i]])
    y <- array(y.data[[j]])
    out <- bc.i(x, y, "gcd")
    expect <- rep(NA_real_, length(out)) |> array()
    expect_equal(
      expect, out
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}



# gcd with datatypes ====

x.data <- list(
  sample(1:100, 10),
  sample(1:100, 10) |> as.double()
)
y.data <- list(
  sample(1:100, 10),
  sample(1:100, 10) |> as.double()
)
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- array(x.data[[i]])
    y <- array(y.data[[j]])
    out <- bc.i(x, y, "gcd")
    expect <- gcd(as.integer(x), as.integer(y)) |> as.array()
    expect_equal(
      expect, out
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}


# gcd with dimensions ====

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


nres <- 5 * 5 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "gcd"

i <- 1L

for(iDimX in c(1, 2, 5, 8, 9)) { # different dimensions for x
  x.dim <- test_make_dims(iDimX)
  x.len <- prod(x.dim)
  for(iDimY in c(1, 2, 5, 8, 9)) { # different dimensions for y
    y.dim <- test_make_dims(iDimY)
    y.len <- prod(y.dim)
    
    x <- array(sample(1:10), x.dim)
    y <- array(sample(1:10), y.dim)
    
    # PREPARE FOR TEST
    tdim <- bc_dim(x, y)
    # print(x)
    # print(y)
    # print(tdim)
    # cat("\n")
    
    
    # DO TESTS BY CASE:
    if(is.null(tdim)) {
      # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
      expected[[i]] <- gcd(as_dbl(drop(x)), as_dbl(drop(y)))
      attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
      out[[i]] <- bc.i(x, y, op)
    }
    else if(length(y) == 1L && length(x) == 1L) {
      # CASE 2: x and y are both scalar arrays
      expected[[i]] <- gcd(as.double(x), as.double(y))
      out[[i]] <- bc.i(x, y, op)
    }
    else if(length(x) == 1L && length(y) > 1L) {
      # CASE 3: x is scalar, y is not
      expected[[i]] <- gcd(as.double(x), rep_dim(as_dbl(y), tdim))
      out[[i]] <- bc.i(x, y, op)
    }
    else if(length(y) == 1L && length(x) > 1L) {
      # CASE 4: y is scalar, x is not
      expected[[i]] <- gcd(rep_dim(as_dbl(x), tdim), as.double(y))
      out[[i]] <- bc.i(x, y, op)
    }
    else {
      # CASE 5: x and y are both non-reducible arrays
      expected[[i]] <- gcd(rep_dim(as_dbl(x), tdim), rep_dim(as_dbl(y), tdim))
      out[[i]] <- bc.i(x, y, op)
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

enumerate <- enumerate + i # count number of tests
# test results:
expect_equal(
  expected, out
)


