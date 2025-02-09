
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


test_make_dimnames <- function(x) {
  out <- lapply(dim(x), \(n)sample(letters, n, replace = TRUE))
  
  # randomly make names of one random dimension NULL
  if(length(out) > 1L && sample(c(TRUE, FALSE), 1L)) {
    out[sample(1:length(out), 1L)] <- list(NULL) 
  }
  
  return(out)
}

# note: for safety, test one nameing argument at a time. Not multiple simultaneously

################################################################################

# test comnames, 1d ====
x <- array(rnorm(10), 10, list(sample(letters, 10)))
y <- array(rnorm(5), 5, list(sample(letters, 5)))
z <- array(rnorm(5), 5, list(sample(letters, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- c(x, y, z)
dim(expected) <- length(expected)
for(i in 1:4) {
  expect_equal(
    bind_array(input, 1L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}



# test comnames, 2d ====


# test comnames, 3d ====


# test name_along, 1d ====


# test name_along, 2d ====


# test name_along, 3d ====


