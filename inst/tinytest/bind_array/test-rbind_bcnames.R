
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


# test comnames, 2d ====
x <- array(rnorm(10), c(10, 5), test_make_dimnames(c(10, 5)))
y <- array(rnorm(5), c(5, 1), test_make_dimnames(c(5, 1))) # will be broadcasted
z <- array(rnorm(5), c(5, 5), test_make_dimnames(c(5, 5)))
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
expected <- rbind(x, y[,rep(1L, 5L)], z) |> unname()
for(i in c(1, 2, 4)) {
  dimnames(expected)[2L] <- dimnames(input[[i]])[2L]
  expect_equal(
    bind_array(input, 1L, name_along = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  enumerate <- enumerate + 1L
}
i = 3
expect_equal(
  bind_array(input, 1L, name_along = FALSE, comnames_from = i),
  expected
) |> errorfun()
enumerate <- enumerate + 1L



# test name_along, 2d ====
x <- array(rnorm(10), c(10, 5), test_make_dimnames(c(10, 5)))
y <- array(rnorm(5), c(5, 1), test_make_dimnames(c(5, 1))) # will be broadcasted
z <- array(rnorm(5), c(5, 5)) # no names
emptyarray <- array(rnorm(0), c(5, 5, 0))
input <- list(emptyarray, x, y, z)
names(input) <- letters[1:4]
expected <- rbind(x, y[, rep(1L, 5L)], z) |> unname()
rownames(expected) <- c(rownames(x), rownames(y), paste0("d.", 1:5))
expect_equal(
  bind_array(input, 1L, name_along = TRUE, comnames_from = NULL),
  expected
)
enumerate <- enumerate + 1L


