
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

sys.source(file.path(getwd(), "source-abind.R"), envir = environment())


# These tests compare bind_array() with a *slightly* adjusted version
# of the battle-tested abind::abind() function.
# all output should be exactly equal.


################################################################################

# along == 1L ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * 3 * 2 * length(datagens)^2)

along <- 1L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iLen in 2:3) {
        for(iNdims in 1:3) {
          
          x.dim <- y.dim <- z.dim <- sample(1:10, iNdims)
          x.dim[along] <- sample(1:10, 1)
          y.dim[along] <- sample(1:10, 1)
          z.dim[along] <- sample(1:10, 1)
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          input <- list(x, y, z)
          input <- input[sample(1:3, iLen)]
          
          expected.lst[[counter]] <- test_abind(input, along = along) |> unname()
          out.lst[[counter]] <- bind_array(input, along = along, name_along = FALSE, comnames_from = NULL)
          counter <- counter + 1L
          
        }
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# along == 2L ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * 3 * 2 * length(datagens)^2)

along <- 2L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iLen in 2:3) {
        for(iNdims in 1:3) {
          
          x.dim <- y.dim <- z.dim <- sample(1:10, iNdims)
          x.dim[along] <- sample(1:10, 1)
          y.dim[along] <- sample(1:10, 1)
          z.dim[along] <- sample(1:10, 1)
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          input <- list(x, y, z)
          input <- input[sample(1:3, iLen)]
          
          expected.lst[[counter]] <- test_abind(input, along = along) |> unname()
          out.lst[[counter]] <- bind_array(input, along = along, name_along = FALSE, comnames_from = NULL)
          counter <- counter + 1L
          
        }
        
      }
    }
  }
}


expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# along == 3L ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * 3 * 2 * length(datagens)^2)

along <- 3L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iLen in 2:3) {
        for(iNdims in 2:4) {
          
          x.dim <- y.dim <- z.dim <- sample(1:10, iNdims)
          x.dim[along] <- sample(1:10, 1)
          y.dim[along] <- sample(1:10, 1)
          z.dim[along] <- sample(1:10, 1)
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          
          input <- list(x, y, z)
          input <- input[sample(1:3, iLen)]
          
          expected.lst[[counter]] <- test_abind(input, along = along) |> unname()
          out.lst[[counter]] <- bind_array(input, along = along, name_along = FALSE, comnames_from = NULL)
          counter <- counter + 1L
          
        }
        
      }
    }
  }
}


expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# along == 0L ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * 3 * length(datagens)^3)

along <- 0L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iLen in 2:3) {
        for(iNdims in 1:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          input <- list(x, y, z)
          input <- input[sample(1:3, iLen)]
          
          expected.lst[[counter]] <- test_abind(input, along = along) |> unname()
          out.lst[[counter]] <- bind_array(input, along = along, name_along = FALSE, comnames_from = NULL)
          counter <- counter + 1L
          
        }
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# revalong == 0L ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * 3 * 2 * length(datagens)^2)

revalong <- 0L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iLen in 2:3) {
        for(iNdims in 1:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          input <- list(x, y, z)
          input <- input[sample(1:3, iLen)]
          
          expected.lst[[counter]] <- test_abind(input, rev.along = revalong) |> unname()
          out.lst[[counter]] <- bind_array(input, along = revalong, rev = TRUE, name_along = FALSE, comnames_from = NULL)
          counter <- counter + 1L
          
        }
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)



# revalong == 1L ====
counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * 3 * length(datagens)^3)

revalong <- 1L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iLen in 2:3) {
        for(iNdims in 1:3) {
          
          x.dim <- y.dim <- z.dim <- empty.dim <- sample(1:10, iNdims)
          x.data <- datagens[[iDataX]]()
          y.data <- datagens[[iDataY]]()
          z.data <- datagens[[iDataY]]()
          
          x <- array(x.data, x.dim)
          y <- array(y.data, y.dim)
          z <- array(z.data, z.dim)
          input <- list(x, y, z)
          input <- input[sample(1:3, iLen)]
          
          expected.lst[[counter]] <- test_abind(input, rev.along = revalong) |> unname()
          out.lst[[counter]] <- bind_array(input, along = revalong, rev = TRUE, name_along = FALSE, comnames_from = NULL)
          counter <- counter + 1L
          
        }
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)

