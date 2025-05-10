
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


test_make_dimnames <- function(x.dim) {
  out <- lapply(x.dim, \(n)sample(letters, n, replace = TRUE))
  
  return(out)
}

# note: for safety, test one nameing argument at a time. Not multiple simultaneously

################################################################################


# test comnames, 3d ====
x <- array(rnorm(10), c(5, 5, 10), test_make_dimnames(c(5, 5, 10)))
y <- array(rnorm(5), c(5, 1, 5), test_make_dimnames(c(5, 1, 5))) # will be broadcasted
z <- array(rnorm(5), c(5, 5, 5), test_make_dimnames(c(5, 5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- array(NA, c(5, 5, 20))
expected[, , 1:10] <- x
expected[, , 11:15] <- y[, rep(1L, 5L), ]
expected[, , 16:20] <- z
for(i in c(1, 2, 4)) {
  dimnames(expected)[1:2] <- dimnames(input[[i]])[1:2]
  expect_equal(
    bind_array(input, 3L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  enumerate <- enumerate + 1L
}


# test name_along, 3d ====
x <- array(rnorm(10), c(5, 5, 10), test_make_dimnames(c(5, 5, 10)))
y <- array(rnorm(5), c(5, 1, 5), test_make_dimnames(c(5, 1, 5))) # will be broadcasted
z <- array(rnorm(5), c(5, 5, 5)) # no names
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
names(input) <- letters[1:4]
expected <- array(NA, c(5, 5, 20))
expected[, , 1:10] <- x
expected[, , 11:15] <- y[, rep(1L, 5L), ]
expected[, , 16:20] <- z
dimnames(expected)[[3]] <- c(dimnames(x)[[3]], dimnames(y)[[3]], paste0("d.", 1:5))

expect_equal(
  bind_array(input, 3L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 1L


