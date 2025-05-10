
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


test_make_dimnames <- function(x) {
  out <- lapply(dim(x), \(n)sample(letters, n, replace = TRUE))
  
  # randomly make names of one random dimension NULL
  if(length(out) > 1L && sample(c(TRUE, FALSE), 1L)) {
    out[sample(1:length(out), 1L)] <- list(NULL) 
  }
  
  return(out)
}



datagens <- list(
  # \() as.raw(sample(1:10)), # ifelse() cannot handle raw, apparently
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


################################################################################

# shared dimensions of size 1 ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * length(datagens)^3)
nDims <- 1L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iDataZ in seq_along(datagens)) {
        
        # make input:
        x.dim <- y.dim <- z.dim <- test_make_dims(nDims)
        x.data <- datagens[[iDataX]]()
        y.data <- datagens[[iDataY]]()
        z.data <- datagens[[iDataZ]]()
        
        x <- array(x.data, x.dim)
        y <- array(y.data, y.dim)
        z <- array(z.data, z.dim)
        emptyarray <- array(numeric(0L), c(3,3,0))
        
        dimnames(x) <- test_make_dimnames(x)
        dimnames(y) <- test_make_dimnames(y)
        dimnames(z) <- test_make_dimnames(z)
        
        # make expected array:
        expected.dim <- c(x.dim, 3L)
        expected.type <- c(x.data, y.data, z.data) |> typeof()
        expected <- vector(expected.type, prod(expected.dim))
        dim(expected) <- expected.dim
        
        expected[, 1] <- x
        expected[, 2] <- y
        expected[, 3] <- z
        
        random <- sample(1:3, 1L)
        if(random == 1L) {
          input <- list(emptyarray, x, y, z)
        }
        else if(random == 2L) {
          input <- list(x, y, z, emptyarray)
        }
        else if(random == 3L) {
          input <- list(x, y, emptyarray, z)
        }
        
        expected.lst[[counter]] <- expected
        out.lst[[counter]] <- bind_array(input, nDims + 1L, name_along = FALSE, comnames_from = NULL)
        
        counter <- counter + 1L
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# shared dimensions of size 2 ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * length(datagens)^3)
nDims <- 2L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iDataZ in seq_along(datagens)) {
        
        # make input:
        x.dim <- y.dim <- z.dim <- test_make_dims(nDims)
        x.data <- datagens[[iDataX]]()
        y.data <- datagens[[iDataY]]()
        z.data <- datagens[[iDataZ]]()
        
        x <- array(x.data, x.dim)
        y <- array(y.data, y.dim)
        z <- array(z.data, z.dim)
        emptyarray <- array(numeric(0L), c(3,3,0))
        
        dimnames(x) <- test_make_dimnames(x)
        dimnames(y) <- test_make_dimnames(y)
        dimnames(z) <- test_make_dimnames(z)
        
        # make expected array:
        expected.dim <- c(x.dim, 3L)
        expected.type <- c(x.data, y.data, z.data) |> typeof()
        expected <- vector(expected.type, prod(expected.dim))
        dim(expected) <- expected.dim
        
        expected[, , 1] <- x
        expected[, , 2] <- y
        expected[, , 3] <- z
        
        random <- sample(1:3, 1L)
        if(random == 1L) {
          input <- list(emptyarray, x, y, z)
        }
        else if(random == 2L) {
          input <- list(x, y, z, emptyarray)
        }
        else if(random == 3L) {
          input <- list(x, y, emptyarray, z)
        }
        
        expected.lst[[counter]] <- expected
        out.lst[[counter]] <- bind_array(input, nDims + 1L, name_along = FALSE, comnames_from = NULL)
        
        counter <- counter + 1L
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# shared dimensions of size 3 ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * length(datagens)^3)
nDims <- 3L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iDataZ in seq_along(datagens)) {
        
        # make input:
        x.dim <- y.dim <- z.dim <- test_make_dims(nDims)
        x.data <- datagens[[iDataX]]()
        y.data <- datagens[[iDataY]]()
        z.data <- datagens[[iDataZ]]()
        
        x <- array(x.data, x.dim)
        y <- array(y.data, y.dim)
        z <- array(z.data, z.dim)
        emptyarray <- array(numeric(0L), c(3,3,0))
        
        
        dimnames(x) <- test_make_dimnames(x)
        dimnames(y) <- test_make_dimnames(y)
        dimnames(z) <- test_make_dimnames(z)
        
        # make expected array:
        expected.dim <- c(x.dim, 3L)
        expected.type <- c(x.data, y.data, z.data) |> typeof()
        expected <- vector(expected.type, prod(expected.dim))
        dim(expected) <- expected.dim
        
        expected[, , , 1] <- x
        expected[, , , 2] <- y
        expected[, , , 3] <- z
        
        random <- sample(1:3, 1L)
        if(random == 1L) {
          input <- list(emptyarray, x, y, z)
        }
        else if(random == 2L) {
          input <- list(x, y, z, emptyarray)
        }
        else if(random == 3L) {
          input <- list(x, y, emptyarray, z)
        }
        
        expected.lst[[counter]] <- expected
        out.lst[[counter]] <- bind_array(input, nDims + 1L, name_along = FALSE, comnames_from = NULL)
        
        counter <- counter + 1L
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



