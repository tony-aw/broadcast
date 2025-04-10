
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

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


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
  for(iData in 1:length(datagens)) {
    yes.data <- datagens[[iData]]()
    no.data <- datagens[[iData]]()
    for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
      yes.dim <- test_make_dims(iDimX)
      yes.len <- prod(yes.dim)
      for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
        no.dim <- test_make_dims(iDimY)
        no.len <- prod(no.dim)
        
        # make data:
        yes <- array(yes.data, dim = yes.dim)
        no <- array(no.data, dim = no.dim)
        tdim <- bc_dim(yes, no)
        cond <- array(sample(c(TRUE, FALSE, NA), 10, TRUE), bc_dim(yes, no))
        
        # DO TESTS BY CASE:
        if(is.null(tdim)) {
          # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
          expected <- ifelse(cond, yes, no)
          attributes(expected) <- NULL # must be a vector if tdim == NULL
          out <- bc_ifelse(cond, yes, no)
        }
        else if(length(yes) == 1L && length(no) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected <- ifelse(cond, yes, no)
          out <- bc_ifelse(cond, yes, no)
        }
        else if(length(yes) == 1L && length(no) > 1L) {
          # CASE 3: x is scalar, y is not
          expected <- ifelse(cond, yes, rep_dim(no, tdim))
          out <- bc_ifelse(cond, yes, no)
        }
        else if(length(yes) > 1L && length(no) == 1L) {
          # CASE 4: y is scalar, x is not
          expected <- ifelse(cond, rep_dim(yes, tdim), no)
          out <- bc_ifelse(cond, yes, no)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected <- ifelse(cond, rep_dim(yes, tdim), rep_dim(no, tdim))
          out <- bc_ifelse(cond, yes, no)
        }
        # END CASES
        
        # ensure correct dimensions:
        dim(expected) <- tdim
        
        # give NULL for missing list:
        if(is.list(out)) {
          expected <- as.list(expected)
          dim(expected) <- tdim
          ind <- which(sapply(expected, \(x)length(x) == 1 && is.na(x)))
          expected[ind] <- list(NULL)
        }
        
        expect_equivalent( # equivalent instead of equal because ifelse() is a bit sloppy sometimes
          expected, out
        ) |> errorfun()
        
        i <- i + 1L
      }
    }
  }
}
enumerate <- enumerate + i # count number of tests
# test results:


# errors ====
expect_error(
  bc_ifelse(letters, LETTERS, letters),
  pattern = "`test` must be a logical array"
)
expect_error(
  bc_ifelse(c(TRUE, FALSE, NA, NA), letters[1:4], 1:4),
  pattern = "`yes` and `no` must be of the same type"
)
expect_error(
  bc_ifelse(c(TRUE, FALSE, NA, NA), as.raw(1:4), as.raw(1:4)),
  pattern = "`yes` and `no` cannot be type of raw"
)
enumerate <- enumerate + 3L
