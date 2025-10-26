#' Internal functions
#'
#'
#'
#'
#'
#'


#' @keywords internal
#' @noRd
.mybadge_casting <- function(x, y, color) {
  filepath <- paste0(gsub(" ", "", x), "-",
                     y, "-", color, ".svg")
  text <- sprintf("\\link[=broadcast_casting]{%s}: %s; ", x, y)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    filepath, toupper(y))
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}



#' @keywords internal
#' @noRd
.test_make_dims <- function(n) {
  
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


#' @keywords internal
#' @noRd
.test_binary <- function(
    bc.fun, base.fun, x.types, y.types,
    nconfigs = 5L, datasize = 100L, ndims = 3L, correctNaN = TRUE, print = FALSE
    ) {
  i <- 1L
  
  nres <- nconfigs * ndims^2 * length(x.types) * length(y.types) # number of tests performed here
  expected <- out <- vector("list", nres)

  for(iSample in 1:nconfigs) { # re-do tests with different random configurations
    
    x.data <- list(
      logical = sample(c(TRUE, FALSE, NA), datasize, TRUE),
      integer = sample(c(-10:10, NA), datasize, TRUE),
      int53 = sample(c(-10:10, NA, NaN, Inf, -Inf), datasize, TRUE),
      double = sample(c(-10.5:10.5, NA, NaN, Inf, -Inf), datasize, TRUE),
      complex = sample(c(-10.5:10.5, NA, NaN, Inf, -Inf), datasize, TRUE) + sample(c(stats::rnorm(10), NA, NaN, Inf, -Inf), datasize, TRUE) * -1i,
      character = sample(c(letters, LETTERS, month.abb, NA), datasize, TRUE),
      raw = sample(as.raw(0:255), datasize, TRUE),
      list = sample(list(letters, month.abb, 1:10, NULL), datasize, TRUE)
    )[x.types]
    
    y.data <- list(
      logical = sample(c(TRUE, FALSE, NA), datasize, TRUE),
      integer = sample(c(-10:10, NA), datasize, TRUE),
      int53 = sample(c(-10:10, NA, NaN, Inf, -Inf), datasize, TRUE),
      double = sample(c(-10.5:10.5, NA, NaN, Inf, -Inf), datasize, TRUE),
      complex = sample(c(-10.5:10.5, NA, NaN, Inf, -Inf), datasize, TRUE) + sample(c(stats::rnorm(10), NA, NaN, Inf, -Inf), datasize, TRUE) * -1i,
      character = sample(c(letters, LETTERS, month.abb, NA), datasize, TRUE),
      raw = sample(as.raw(0:255), datasize, TRUE),
      list = sample(list(letters, month.abb, 1:10, NULL), datasize, TRUE)
    )[y.types]
    
    for(iDimX in sample(1:8, ndims)) { # different dimensions for x
      x.dim <- .test_make_dims(iDimX)
      x.len <- prod(x.dim)
      for(iDimY in sample(1:8, ndims)) { # different dimensions for y
        y.dim <- .test_make_dims(iDimY)
        y.len <- prod(y.dim)
        
        for(iDataX in 1:length(x.data)) { # different data types for x
          x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
          for(iDataY in 1:length(y.data)) { # different data types for y
            y <- array(y.data[[iDataY]][1:y.len], dim = y.dim)
            
            # PREPARE FOR TEST
            tdim <- bc_dim(x, y)
            
            if(print) {
              print(x)
              print(y)
              print(tdim)
              cat("\n")
              cat("dim(x) = ", dim(x), "\n")
              cat("dim(y) = ", dim(y), "\n")
            }
            
            # DO TESTS BY CASE:
            if(is.null(tdim)) {
              # CASE 1: result has no dimensions (for ex. when x and y are both scalars)
              expected[[i]] <- base.fun(drop(x), drop(y))
              dim
              # attributes(expected[[i]]) <- NULL # must be a vector if tdim == NULL
              out[[i]] <- bc.fun(x, y)
            }
            else if(length(y) == 1L && length(x) == 1L) {
              # CASE 2: x and y are both scalar arrays
              expected[[i]] <- base.fun(undim(x), undim(y))
              out[[i]] <- bc.fun(x, y)
            }
            else if(length(x) == 1L && length(y) > 1L) {
              # CASE 3: x is scalar, y is not
              expected[[i]] <- base.fun(undim(x), rep_dim(y, tdim))
              out[[i]] <- bc.fun(x, y)
            }
            else if(length(y) == 1L && length(x) > 1L) {
              # CASE 4: y is scalar, x is not
              expected[[i]] <- base.fun(rep_dim(x, tdim), undim(y))
              out[[i]] <- bc.fun(x, y)
            }
            else {
              # CASE 5: x and y are both non-reducible arrays
              expected[[i]] <- base.fun(rep_dim(x, tdim), rep_dim(y, tdim))
              out[[i]] <- bc.fun(x, y)
            }
            # END CASES
            
            if(correctNaN) {
              # R is sometimes inconsistent whether it returns NA or NaN
              # for example: NaN + NaN = NA, but NaN - NaN = NaN
              # the 'broadcast' package prefers to remain consistent in all NA/NaN cases
              # the following code is meant to ensure NaN results turn to NA, like 'broadcast' does
              ind.NaN <- is.nan(expected[[i]])
              expected[[i]][ind.NaN] <- .return_missing(expected[[i]][ind.NaN])
              ind.NaN <- is.nan(out[[i]])
              out[[i]][ind.NaN] <- .return_missing(out[[i]][ind.NaN])
            }
            
            
            # ensure correct dimensions:
            dim(expected[[i]]) <- tdim
            
            # expect_equal(
            #   expected[[i]],
            #   out[[i]]
            # ) |> errorfun()
            
            i <- i + 1L
          }
        }
      }
    }
  }
  
  return(list(expected = expected, out = out, i = i))
  
}
  