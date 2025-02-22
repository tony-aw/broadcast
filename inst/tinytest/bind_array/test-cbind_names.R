
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

# note: for safety, test one naming argument at a time. Not multiple simultaneously

################################################################################

# test comnames, 1d ====
x <- array(rnorm(5), 5, list(sample(letters, 5)))
y <- array(rnorm(5), 5, list(sample(letters, 5)))
z <- array(rnorm(5), 5, list(sample(letters, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- cbind(x, y, z) |> unname()
for(i in 1:4) {
  dimnames(expected)[1L] <- dimnames(input[[i]])[1L] 
  expect_equal(
    bind_array(input, 2L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  
  enumerate <- enumerate + 1L
}


# test comnames, 2d ====
x <- array(rnorm(10), c(5, 10), test_make_dimnames(c(5, 10)))
y <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- cbind(x, y, z) |> unname()
for(i in 1:4) {
  dimnames(expected)[1L] <- dimnames(input[[i]])[1L]
  expect_equal(
    bind_array(input, 2L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  
  enumerate <- enumerate + 1L
}


# test name_along, 1d ====
x <- array(rnorm(10), 10, list(sample(letters, 10)))
y <- array(rnorm(5), 5)
z <- array(rnorm(5), 5, list(sample(letters, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
names(input) <- letters[1:4]
expected <- c(x, y, z)
dim(expected) <- length(expected)
names(expected) <- c(names(x), paste0("c.", 1:5), names(z))
expect_equal(
  bind_array(input, 1L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 1L


# test name_along, 2d ====
x <- array(rnorm(10), c(5, 10), test_make_dimnames(c(5, 10)))
y <- array(rnorm(5), c(5, 5))
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
names(input) <- letters[1:4]
expected <- cbind(x, y, z) |> unname()
colnames(expected) <- c(colnames(x), paste0("c.", 1:5), colnames(z))
expect_equal(
  bind_array(input, 2L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 1L


