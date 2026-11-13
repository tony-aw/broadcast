
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_make_dims <- broadcast:::.test_make_dims
.test_make_dimpair_broadcastable <- broadcast:::.test_make_dimpair_broadcastable

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


# main test ====

op <- function(x, y) {
  c(length(x) == length(y), typeof(x) == typeof(y))
}

nres <- 5 * 3 * length(datagens)* 4 * 4
expected <- out <- vector("list", nres)

i <- 1L
for(iSample in 1:5) { # re-do tests with different random configurations
  for(iTestType in c("logical", "integer", "raw")) {
    for(iData in 1:length(datagens)) {
      yes.data <- datagens[[iData]]()
      no.data <- datagens[[iData]]()
      for(iDimX in c(1, 2, 5, 8)) { # different dimensions for x
        yes.dim <- sample(c(1L, 1L, 2:5), iDimX, TRUE)
        for(iDimY in c(1, 2, 5, 8)) { # different dimensions for y
          no.dim <- sample(c(1L, 1L, 2:5), iDimX, TRUE)
          
          yesno.dim <- .test_make_dimpair_broadcastable(yes.dim, no.dim)
          yes.dim <- yesno.dim[[1L]]
          no.dim <- yesno.dim[[2L]]
          yes.len <- prod(yes.dim)
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
            cond <- suppressWarnings(as_raw(cond))
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
          
          # in expected[[i]], give NULL instead of NA for missing list-elements:
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


# numeric types leniency ====
x <- array(sample(1:100), c(1, 100)) |> as_int()
y <- array(sample(1:100), c(100, 1)) |> as_int()
test <- bc.d(x, y, ">")
expect_equal(
  bc_ifelse(test, as_dbl(x), y),
  bc_ifelse(test, x, as_dbl(y))
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

