

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen

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



enumerate <- 0L



# basic tests ====

expect_equal(
  bc.str(array("hello"), array("hello"), "levenshtein"),
  array(0L)
)
expect_equal(
  bc.str(array("kitten"), array("sitting"), "levenshtein"),
  array(3L)
)
expect_equal(
  bc.str(array("kitten"), array("kkkitten"), "levenshtein"),
  array(2L)
)
expect_equal(
  bc.str(array("hello"), array("hellok"), "levenshtein"),
  array(1L)
)
expect_equal(
  bc.str(array("helklo"), array("hello"), "levenshtein"),
  array(1L)
)


expect_equal(
  bc.str(month.name, array(month.abb, c(1, 12)), "levenshtein"),
  adist(month.name, month.abb)
)

expect_equal(
  bc.str(array("hello"), array("hello"), "lcss"),
  array(5L)
)
expect_equal(
  bc.str(array("kitten"), array("sitting"), "lcss"),
  array(3L)
)
expect_equal(
  bc.str(array("kitten"), array("kkkitten"), "lcss"),
  array(6L)
)
expect_equal(
  bc.str(array("hello"), array("hellok"), "lcss"),
  array(5L)
)
expect_equal(
  bc.str(array("helklo"), array("hello"), "lcss"),
  array(3L)
)


enumerate <- enumerate + 11L


# NA tests ====
expect_equal(
  bc.str(month.abb, rep(NA_character_, 12), "levenshtein"),
  rep(NA_integer_, 12)
)
expect_equal(
  bc.str(rep(NA_character_, 12), month.abb, "levenshtein"),
  rep(NA_integer_, 12)
)
expect_equal(
  bc.str(rep(NA_character_, 26), rep(NA_character_, 26), "levenshtein"),
  rep(NA_integer_, 26)
)
expect_equal(
  bc.str(month.abb, rep(NA_character_, 12), "lcss"),
  rep(NA_integer_, 12)
)
expect_equal(
  bc.str(rep(NA_character_, 12), month.abb, "lcss"),
  rep(NA_integer_, 12)
)
expect_equal(
  bc.str(rep(NA_character_, 26), rep(NA_character_, 26), "lcss"),
  rep(NA_integer_, 26)
)
enumerate <- enumerate + 6L





# empty strings tests ====
x <- rep("", 10)
y <- rep("", 10)
expect_equal(
  bc.str(x, y, "levenshtein"),
  rep(0L, 10L)
)
expect_equal(
  bc.str(x, y, "lcss"),
  rep(0L, 10L)
)
enumerate <- enumerate + 2L


# dimensional tests ====

nres <- 5 * 5 # number of tests performed here
expected <- out <- vector("list", nres)
op <- "levenshtein"

i <- 1L
x.data <- sample(letters)
y.data <- sample(letters)
basefun <- function(x, y) {
  out <- mapply(adist, x, y)
  dim(out) <- bc_dim(x, y)
  return(out)
}

for(iDimX in sample(1:8, 3L)) { # different dimensions for x
  x.dim <- test_make_dims(iDimX)
  x.len <- prod(x.dim)
  for(iDimY in sample(1:8, 3L)) { # different dimensions for y
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
      expected[[i]] <- basefun(as_chr(drop(x)), as_chr(drop(y)))
      attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
      out[[i]] <- bc.str(x, y, op)
    }
    else if(length(y) == 1L && length(x) == 1L) {
      # CASE 2: x and y are both scalar arrays
      expected[[i]] <- basefun(as.character(x), as.character(y))
      out[[i]] <- bc.str(x, y, op)
    }
    else if(length(x) == 1L && length(y) > 1L) {
      # CASE 3: x is scalar, y is not
      expected[[i]] <- basefun(as.character(x), rep_dim(as_chr(y), tdim))
      out[[i]] <- bc.str(x, y, op)
    }
    else if(length(y) == 1L && length(x) > 1L) {
      # CASE 4: y is scalar, x is not
      expected[[i]] <- basefun(rep_dim(as_chr(x), tdim), as.character(y))
      out[[i]] <- bc.str(x, y, op)
    }
    else {
      # CASE 5: x and y are both non-reducible arrays
      expected[[i]] <- basefun(rep_dim(as_chr(x), tdim), rep_dim(as_chr(y), tdim))
      out[[i]] <- bc.str(x, y, op)
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



# attributes tests ====
bc.fun <- function(x, y) { bc.str(x, y, "levenshtein")}
types <- "character"
res <- .test_binary_class(bc.fun, types, types)
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
expect_false(
  identical(res$expected_ma, res$out_ma)
)
enumerate <- enumerate + res$i


# zerolen tests ====
bc.fun <- function(x, y) { bc.str(x, y, "levenshtein")}
res <- .test_binary_zerolen(bc.fun, is.integer, types, types)
expect_true(all(res$is_OK_type))
expect_equal(
  res$expected_bc, res$out_bc
)
expect_false(
  identical(res$expected_comm, res$out_comm)
)
enumerate <- enumerate + res$i




