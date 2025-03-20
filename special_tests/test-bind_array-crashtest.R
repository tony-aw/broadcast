
library(broadcast)
library(tinytest)

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
  # \() as.raw(sample(1:10)), # ifelse() cannot handle raw, apparently
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


# These tests check if bind_array() can run
# in all circumstances that it should be able to run
# without errors, warnings, messages, and definitely without crashes


################################################################################

# along == 1L ====
along <- 1L

for(iDataX in seq_along(datagens)) {
  for(iDataY in seq_along(datagens)) {
    for(iNdims in 1:3) {
      for(iNmal in c(TRUE, FALSE)) {
        for(iLen in 2:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          x.dim[along] <- sample(1:10, 1)
          y.dim[along] <- sample(1:10, 1)
          z.dim[along] <- sample(1:10, 1)
          y.dim[sample(1:length(y.dim), 1L)] <- 1L # make y  broadcast
          empty.dim[along] <- 0L
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          emptyarray <- vector(typeof(x), 0L)
          dim(emptyarray) <- empty.dim
          
          input <- list(x, y, emptyarray)
          input <- input[sample(1:3, iLen)]
          
          expect_silent(
            bind_array(input, along = along, name_along = iNmal, comnames_from = sample(1:iLen, 1L))
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
        }
      }
    }
  }
}


# along == 2L ====
along <- 2L

for(iDataX in seq_along(datagens)) {
  for(iDataY in seq_along(datagens)) {
    for(iNdims in 1:3) {
      for(iNmal in c(TRUE, FALSE)) {
        for(iLen in 2:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          x.dim[along] <- sample(1:10, 1)
          y.dim[along] <- sample(1:10, 1)
          z.dim[along] <- sample(1:10, 1)
          y.dim[sample(1:length(y.dim), 1L)] <- 1L # make y  broadcast
          empty.dim[along] <- 0L
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          emptyarray <- vector(typeof(x), 0L)
          dim(emptyarray) <- empty.dim
          
          input <- list(x, y, emptyarray)
          input <- input[sample(1:3, iLen)]
          
          expect_silent(
            bind_array(input, along = along, name_along = iNmal, comnames_from = sample(1:iLen, 1L))
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
        }
      }
    }
  }
}



# along == 3L ====
along <- 3L

for(iDataX in seq_along(datagens)) {
  for(iDataY in seq_along(datagens)) {
    for(iNdims in 2:4) {
      for(iNmal in c(TRUE, FALSE)) {
        for(iLen in 2:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          x.dim[along] <- sample(1:10, 1)
          y.dim[along] <- sample(1:10, 1)
          z.dim[along] <- sample(1:10, 1)
          y.dim[sample(1:length(y.dim), 1L)] <- 1L # make y  broadcast
          empty.dim[along] <- 0L
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          emptyarray <- vector(typeof(x), 0L)
          dim(emptyarray) <- empty.dim
          
          input <- list(x, y, emptyarray)
          input <- input[sample(1:3, iLen)]
          
          expect_silent(
            bind_array(input, along = along, name_along = iNmal, comnames_from = sample(1:iLen, 1L))
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
        }
      }
    }
  }
}


# along == 0L ====
along <- 0L

for(iDataX in seq_along(datagens)) {
  for(iDataY in seq_along(datagens)) {
    for(iNdims in 1:3) {
      for(iNmal in c(TRUE, FALSE)) {
        for(iLen in 2:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          y.dim[sample(1:length(y.dim), 1L)] <- 1L # make y  broadcast
          empty.dim[1L] <- 0L
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          emptyarray <- vector(typeof(x), 0L)
          dim(emptyarray) <- empty.dim
          
          input <- list(x, y, emptyarray)
          input <- input[sample(1:3, iLen)]
          
          expect_silent(
            bind_array(input, along = along, name_along = iNmal, comnames_from = sample(1:iLen, 1L))
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
        }
      }
    }
  }
}



# revalong == 0L ====
along <- 0L
for(iDataX in seq_along(datagens)) {
  for(iDataY in seq_along(datagens)) {
    for(iNdims in 1:3) {
      for(iNmal in c(TRUE, FALSE)) {
        for(iLen in 2:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          y.dim[sample(1:length(y.dim), 1L)] <- 1L # make y  broadcast
          empty.dim[1L] <- 0L
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          emptyarray <- vector(typeof(x), 0L)
          dim(emptyarray) <- empty.dim
          
          input <- list(x, y, emptyarray)
          input <- input[sample(1:3, iLen)]
          
          expect_silent(
            bind_array(input, along = along, rev = TRUE, name_along = iNmal, comnames_from = sample(1:iLen, 1L))
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
        }
      }
    }
  }
}

