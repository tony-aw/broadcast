
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 4 or 16:
  out <- lapply(1:n, \(n)sample(c(4, 16), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}


test_make_dimnames <- function(x.dim) {
  out <- lapply(x.dim, \(n) sample(letters, n, TRUE))
  if(length(out) > 1) {
    out[sample(1:length(out), 1)] <- list(NULL)
  }
  return(out)
}


test_make_fillval <- function(x) {
  if(is.raw(x)) {
    fillvalue <- as.raw(0L)
  }
  else if(is.atomic(x)) {
    fillvalue <- NA
  }
  else if(is.recursive(x)) {
    fillvalue <- list(NULL)
  }
}


datagens <- list(
  \() as.raw(sample(1:10)),
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)

