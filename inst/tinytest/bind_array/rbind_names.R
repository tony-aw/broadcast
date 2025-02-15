
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


test_make_dimnames <- function(x.dim) {
  out <- lapply(x.dim, \(n)sample(letters, n, replace = TRUE))
  
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
x <- array(rnorm(10), c(10, 5), test_make_dimnames(c(10, 5)))
y <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- rbind(x, y, z) |> unname()
for(i in 1:4) {
  dimnames(expected)[2L] <- dimnames(input[[i]])[2L]
  expect_equal(
    bind_array(input, 1L, name_along = FALSE, comnames_from = i),
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
x <- array(rnorm(10), c(10, 5), test_make_dimnames(c(10, 5)))
y <- array(rnorm(5), c(5, 5))
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
names(input) <- letters[1:4]
expected <- rbind(x, y, z) |> unname()
rownames(expected) <- c(rownames(x), paste0("c.", 1:5), rownames(z))
expect_equal(
  bind_array(input, 1L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 1L


