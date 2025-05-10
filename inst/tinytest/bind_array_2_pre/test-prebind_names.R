
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
expected <- array(NA, dim = c(3, 5))
expected[1, ] <- x
expected[2, ] <- y
expected[3, ] <- z
for(i in 1:4) {
  dimnames(expected)[[2]] <- dimnames(input[[i]])[[1]]
  expect_equal(
    bind_array(input, 0L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  
  enumerate <- enumerate + 1L
}


# test comnames, 2d ====
x <- array(rnorm(10), c(5, 5), test_make_dimnames(c(5, 5)))
y <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- array(NA, dim = c(3, 5, 5))
expected[1, , ] <- x
expected[2, , ] <- y
expected[3, , ] <- z
for(i in 1:4) {
  dimnames(expected)[2:3] <- dimnames(input[[i]])
  expect_equal(
    bind_array(input, 0L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  
  enumerate <- enumerate + 1L
}


# test name_along, 1d ====
x <- array(rnorm(5), 5, list(sample(letters, 5)))
y <- array(rnorm(5), 5)
z <- array(rnorm(5), 5, list(sample(letters, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(x, emptyarray, y, z)
names(input) <- letters[1:4]
expected <- array(NA, dim = c(3, 5))
expected[1, ] <- x
expected[2, ] <- y
expected[3, ] <- z
dimnames(expected) <- list(letters[c(1, 3:4)], NULL)
expect_equal(
  bind_array(input, 0L, name_along = TRUE, comnames_from = NULL),
  expected
)
dimnames(expected) <- list(paste0("X", 1:3), NULL)
expect_equal(
  bind_array(unname(input), 0L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 2L


# test name_along, 2d ====
x <- array(rnorm(10), c(5, 5), test_make_dimnames(c(5, 5)))
y <- array(rnorm(5), c(5, 5))
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(x, emptyarray, y, z)
names(input) <- letters[1:4]
expected <- array(NA, dim = c(3, 5, 5))
expected[1, , ] <- x
expected[2, , ] <- y
expected[3, , ] <- z
dimnames(expected) <- list(letters[c(1, 3:4)], NULL, NULL)
expect_equal(
  bind_array(input, 0L, name_along = TRUE, comnames_from = NULL),
  expected
)
dimnames(expected) <- list(paste0("X", 1:3), NULL, NULL)
expect_equal(
  bind_array(unname(input), 0L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 2L


