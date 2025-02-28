
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

along <- 2L

counter <- 1L
expected.lst <- out.lst <- vector("list", 5 * length(datagens)^3)

nDims <- 3L
for(iSample in 1:5) {
  for(iDataX in seq_along(datagens)) {
    for(iDataY in seq_along(datagens)) {
      for(iDataZ in seq_along(datagens)) {
        
        # make input:
        x.dim <- y.dim <- z.dim <- test_make_dims(nDims)
        x.dim <- x.dim
        y.dim <- y.dim
        x.dim[along] <- sample(1:10, 1)
        y.dim[along] <- sample(1:10, 1)
        z.dim[along] <- sample(1:10, 1)
        x.data <- datagens[[iDataX]]()
        y.data <- datagens[[iDataY]]()
        z.data <- datagens[[iDataZ]]()
        
        # here there's colbinding, so keep object.dim[2] intact
        x <- array(x.data, c(1, x.dim[2])) |> array_replicate(x.dim)
        y <- array(y.data, c(1, y.dim[2:3])) |> array_replicate(y.dim)
        z <- array(z.data, z.dim)
        emptyarray <- array(numeric(0L), c(3,3,0))
        
        
        # make expected array:
        expected.dim <- x.dim
        expected.dim[along] <- x.dim[along] + y.dim[along] + z.dim[along]
        expected.type <- c(x.data, y.data, z.data) |> typeof()
        expected <- vector(expected.type, prod(expected.dim))
        dim(expected) <- expected.dim
        
        start <- 1
        end <- x.dim[along]
        expected[, start:end, ] <- x
        
        start <- start + x.dim[along]
        end <- end + y.dim[along]
        expected[, start:end, ] <- y
        
        start <- start + y.dim[along]
        end <- end + z.dim[along]
        expected[, start:end, ] <- z
        
        # test:
        random <- sample(1:3, 1L)
        if(random == 1L) {
          input <- list(emptyarray, x[1, , 1, drop=FALSE], y[1,,, drop=FALSE], z)
        }
        else if(random == 2L) {
          input <- list(x[1, , 1, drop=FALSE], y[1,,, drop=FALSE], z, emptyarray)
        }
        else if(random == 3L) {
          input <- list(emptyarray, x[1, , 1, drop=FALSE], y[1,,, drop=FALSE], emptyarray, z)
        }
        

        expected.lst[[counter]] <- expected
        out.lst[[counter]] <- bind_array(input, along, max_bc = 2L)
        
        
        counter <- counter + 1L
        
      }
    }
  }
}

expect_equal(
  out.lst, expected.lst
)
enumerate <- enumerate + length(out.lst)

