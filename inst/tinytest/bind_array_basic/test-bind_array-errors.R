
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


# empty input errors ====
expect_error(
  bind_array(list(), 1L),
  pattern = "`input` must be a list with at least 2 elements"
)
expect_error(
  bind_array(list(array(1:10)), 1L),
  pattern = "`input` must be a list with at least 2 elements"
)
expect_error(
  bind_array(list(array(numeric(0L)), array(numeric(0L))), 1L),
  pattern = "`input` must contain at least one non-zero array/vector"
)
enumerate <- enumerate + 3L



# arg along errors ====
input <- list(
  array(1:10), array(1:10)
)
expect_error(
  bind_array(input, 1:10),
  pattern = "`along` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, 1:10, TRUE),
  pattern = "`along` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, "a"),
  pattern = "`along` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, "a", TRUE),
  pattern = "`along` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, -1),
  pattern = "`along` may not be negative or larger than 16",
  fixed = TRUE
)
expect_error(
  bind_array(input, -1, TRUE),
  pattern = "`along` may not be negative or larger than 16",
  fixed = TRUE
)
expect_error(
  bind_array(input, 17),
  pattern = "`along` may not be negative or larger than 16",
  fixed = TRUE
)
expect_error(
  bind_array(input, 17, TRUE),
  pattern = "`along` may not be negative or larger than 16",
  fixed = TRUE
)
expect_error(
  bind_array(input, 3L),
  pattern = "`along` out of bounds",
  fixed = TRUE
)
expect_error(
  bind_array(input, 3L, TRUE),
  pattern = "`along` out of bounds",
  fixed = TRUE
)
enumerate <- enumerate + 10L


# arg along errors ====
input <- list(
  array(1:10), array(1:10)
)
expect_error(
  bind_array(input, 1L, ndim2bc = "a"),
  pattern = "`ndim2bc` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, 1L, ndim2bc = 1:10),
  pattern = "`ndim2bc` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, 1L, ndim2bc = NA_integer_),
  pattern = "`ndim2bc` must be an integer scalar",
  fixed = TRUE
)
expect_error(
  bind_array(input, 1L, ndim2bc = -1),
  pattern = "`ndim2bc` must be non-negative",
  fixed = TRUE
)
enumerate <- enumerate + 4L



# non-conformable arrays ====
x <- array(1:10, c(2, 10))
y <- array(1:20, c(10, 2))
input <- list(x, y)

expect_error(
  bind_array(input, 2L),
  pattern = "arrays are not conformable for binding"
)

enumerate <- enumerate + 1L



# broadcasting will exceed maximum size ====
maxint <- 2^53 + 1L
n <- ceiling(sqrt(maxint))
x <- array(c(TRUE, FALSE, NA), c(n, 1))
y <- array(c(TRUE, FALSE, NA), c(1, n))
input <- list(x, y)
expect_error(
  bind_array(input, 1L, ndim2bc = 2L),
  pattern = "output will exceed maximum vector size"
)
expect_error(
  bind_array(input, 2L, ndim2bc = 2L),
  pattern = "output will exceed maximum vector size"
)
enumerate <- enumerate + 2L


# naming args errors ====
x <- cbind(1:10, 1:10)
dimnames(x) <- list(letters[1:10], LETTERS[1:2])
input <- list(
  x, array(1:10), array(numeric(0L))
)
expect_error(
  bind_array(input, 2L, name_along = NA),
  pattern = "`name_along` must be `TRUE` or `FALSE`",
  fixed = TRUE
)
expect_error(
  bind_array(input, 2L, name_along = c(TRUE, FALSE)),
  pattern = "`name_along` must be `TRUE` or `FALSE`",
  fixed = TRUE
)
expect_error(
  bind_array(input, 2L, comnames_from = 1:10),
  pattern = "`comnames_from` must be an integer scalar or `NULL`"
)
expect_error(
  bind_array(input, 2L, comnames_from = NA_integer_),
  pattern = "`comnames_from` must be an integer scalar or `NULL`"
)
expect_error(
  bind_array(input, 2L, comnames_from = 0L),
  pattern = "`comnames_from` out of bounds"
)
expect_error(
  bind_array(input, 2L, comnames_from = -1),
  pattern = "`comnames_from` out of bounds"
)
expect_error(
  bind_array(input, 2L, comnames_from = 4L),
  pattern = "`comnames_from` out of bounds"
)
expect_silent(
  bind_array(input, 2L, comnames_from = 3L)
)
enumerate <- enumerate + 8L

