
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

gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))


# plus ====

for(iSample in 1:5) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE), # double
    sample(gen() + gen() * -1i, 100, TRUE) # complex
  )
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDataX in 1:length(x.data)) { # different data types for x
      x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
      y <- broadcast:::.as.broadcaster(x)
      expected <- + x
      out <- + y
      
      expect_equivalent(
        expected,
        unclass(out)
      ) |> errorfun()
      enumerate <- enumerate + 1L
    }
  }
}



# min ====

for(iSample in 1:5) { # re-do tests with different random configurations
  x.data <- list(
    sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
    sample(c(-10:10, NA), 100, TRUE), # integer
    sample(c(rnorm(10), NA, NaN, Inf, -Inf), 100, TRUE), # double
    sample(gen() + gen() * -1i, 100, TRUE) # complex
    
    
  )
  for(iDimX in sample(1:8, 3L)) { # different dimensions for x
    x.dim <- test_make_dims(iDimX)
    x.len <- prod(x.dim)
    for(iDataX in 1:length(x.data)) { # different data types for x
      x <- array(x.data[[iDataX]][1:x.len], dim = x.dim)
      y <- broadcast:::.as.broadcaster(x)
      expected <- - x
      out <- - y
      
      expect_equivalent(
        expected,
        unclass(out)
      ) |> errorfun()
      enumerate <- enumerate + 1L
    }
  }
}

