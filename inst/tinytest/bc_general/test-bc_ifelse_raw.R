
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

i <- 1L


test_ifelse <- function(cond, yes, no) {
  n <- length(cond)
  out <- raw(n)
  ind1 <- which(as.logical(cond))
  ind0 <- which(!as.logical(cond))
  if(length(yes) == 1L) {
    yes <- rep_len(yes, n)
  }
  if(length(no) == 1L) {
    no <- rep_len(no, n)
  }
  if(length(ind1)) {
    out[ind1] <- yes[ind1]
  }
  if(length(ind0)) {
    out[ind0] <- no[ind0]
  }
  
  return(out)
}

for(iSample in 1:5) { # re-do tests with different random configurations
  yes.data <- as.raw(sample(0:255))
  no.data <- as.raw(sample(0:255))
  for(iTestType in c("logical", "integer", "raw")) {
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
        cond <- array(sample(c(TRUE, FALSE), 10, TRUE), bc_dim(yes, no))
        
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
          expected <- test_ifelse(cond, yes, no)
          attributes(expected) <- NULL # must be a vector if tdim == NULL
          out <- bc_ifelse(cond, yes, no)
        }
        else if(length(yes) == 1L && length(no) == 1L) {
          # CASE 2: x and y are both scalar arrays
          expected <- test_ifelse(cond, yes, no)
          out <- bc_ifelse(cond, yes, no)
        }
        else if(length(yes) == 1L && length(no) > 1L) {
          # CASE 3: x is scalar, y is not
          expected <- test_ifelse(cond, yes, rep_dim(no, tdim))
          out <- bc_ifelse(cond, yes, no)
        }
        else if(length(yes) > 1L && length(no) == 1L) {
          # CASE 4: y is scalar, x is not
          expected <- test_ifelse(cond, rep_dim(yes, tdim), no)
          out <- bc_ifelse(cond, yes, no)
        }
        else {
          # CASE 5: x and y are both non-reducible arrays
          expected <- test_ifelse(cond, rep_dim(yes, tdim), rep_dim(no, tdim))
          out <- bc_ifelse(cond, yes, no)
        }
        # END CASES
        
        # ensure correct dimensions:
        dim(expected) <- tdim
        
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


expect_error(
  bc_ifelse(
    sample(c(TRUE, FALSE, NA), 256, TRUE),
    sample(0:255) |> as.raw(),
    sample(0:255) |> as.raw()
  ),
  pattern = "NA condition not supported for type of `raw`"
)
enumerate <- enumerate + 1L

