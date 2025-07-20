
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
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


# main test ====
i <- 1L

op <- function(x, y) {
  c(length(x) == length(y), typeof(x) == typeof(y))
}

nres <- 5 * 3 * length(datagens)^2 * 4 * 4
expected <- out <- list()

for(iSample in 1:5) { # re-do tests with different random configurations
  for(iTestType in c("logical", "integer", "raw")) {
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
          
          if(iTestType == "integer") {
            cond <- as_int(cond)
          }
          if(iTestType == "raw") {
            cond <- as_raw(cond)
          }
          if(sample(0:1, 1)) {
            dim(cond) <- NULL # randomly make `test` argument without dimensions
          }
          
          # DO TESTS BY CASE:
          if(is.null(tdim)) {
            # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
            expected[[i]] <- ifelse(as.logical(cond), yes, no)
            attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
            out[[i]] <- bc_ifelse(cond, yes, no)
          }
          else if(length(yes) == 1L && length(no) == 1L) {
            # CASE 2: x and y are both scalar arrays
            expected[[i]] <- ifelse(as.logical(cond), yes, no)
            out[[i]] <- bc_ifelse(cond, yes, no)
          }
          else if(length(yes) == 1L && length(no) > 1L) {
            # CASE 3: x is scalar, y is not
            expected[[i]] <- ifelse(as.logical(cond), yes, rep_dim(no, tdim))
            out[[i]] <- bc_ifelse(cond, yes, no)
          }
          else if(length(yes) > 1L && length(no) == 1L) {
            # CASE 4: y is scalar, x is not
            expected[[i]] <- ifelse(cond, rep_dim(yes, tdim), no)
            out[[i]] <- bc_ifelse(cond, yes, no)
          }
          else {
            # CASE 5: x and y are both non-reducible arrays
            expected[[i]] <- ifelse(as.logical(cond), rep_dim(yes, tdim), rep_dim(no, tdim))
            out[[i]] <- bc_ifelse(cond, yes, no)
          }
          # END CASES
          
          # ensure correct dimensions:
          dim(expected[[i]]) <- tdim
          
          # give NULL for missing list:
          if(is.list(out[[i]])) {
            expected[[i]] <- as.list(expected[[i]])
            dim(expected[[i]]) <- tdim
            ind <- which(sapply(expected[[i]], \(x)length(x) == 1 && is.na(x)))
            expected[[i]][ind] <- list(NULL)
          }
          
          
          i <- i + 1L
        }
      }
    }
  }
  
}

# test results:
expect_equivalent( # equivalent instead of equal because ifelse() is a bit sloppy sometimes
  expected, out
)
enumerate <- enumerate + i # count number of tests


# numeric types leniancy ====
x <- array(sample(1:100), c(1, 100)) |> as_int()
y <- array(sample(1:100), c(100, 1)) |> as_int()
test <- bc.d(x, y, ">")
expect_equal(
  bc_ifelse(test, as_dbl(x), y),
  bc_ifelse(test, x, as_dbl(y))
)
enumerate <- enumerate + 1L


# dimnames ====
x <- array(1:10)
y <- array(1:10, c(1, 10))
test <- bc.d(x, y, ">")
dimnames(test) <- list(letters[1:10], LETTERS[1:10])

expect_equal(
  bc_ifelse(test, x, y) |> dimnames(),
  dimnames(test)
)


dim(test) <- length(test)
dimnames(test) <- list(sample(letters, length(test), TRUE))

expect_equal(
  bc_ifelse(test, x, y) |> dimnames(),
  NULL
)

enumerate <- enumerate + 1L



# errors ====
expect_error(
  bc_ifelse(letters, LETTERS, letters),
  pattern = "unsupported type given for `test`"
)
expect_error(
  bc_ifelse(c(TRUE, FALSE, NA, NA), letters[1:4], 1:4),
  pattern = "`yes` and `no` must be of the same type"
)
expect_error(
  bc_ifelse(1:10, 1:4, 1:4),
  pattern = "`test` of incorrect length"
)
enumerate <- enumerate + 3L
