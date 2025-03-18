
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
  bind_mat(list(), 1L),
  pattern = "`input` cannot be an empty list"
)
expect_error(
  bind_mat(list(array(numeric(0L))), 1L),
  pattern = "`input` must contain at least one non-zero array/vector"
)
enumerate <- enumerate + 2L



# arg along errors ====
input <- list(
  array(1:10), array(1:10)
)
expect_error(
  bind_mat(input, 1:10),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)
expect_error(
  bind_mat(input, "a"),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)
expect_error(
  bind_mat(input, -1),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)
expect_error(
  bind_mat(input, 3, TRUE),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)

enumerate <- enumerate + 4L



# fractional recycling not allowed ====
x <- 1:10
y <- 1:3
input <- list(x, y)

expect_error(
  bind_mat(input, 2L),
  pattern = "fractional recycling not allowed"
)

enumerate <- enumerate + 1L


# non-matrix input ===
x <- array(1:27, c(3,3,3))
y <- 1:3
input <- list(x, y)

expect_error(
  bind_mat(input, 2L),
  pattern = "use `bind_array()` to bind arrays with more than 2 dimensions",
  fixed = TRUE
)

x <- data.frame(1:3)
y <- 1:3
input <- list(x, y)
expect_error(
  bind_mat(input, 2L),
  pattern = "use `bind_dt to bind data.frame-like objects",
  fixed = TRUE
)
enumerate <- enumerate + 2L



# naming args errors ====
x <- cbind(1:10, 1:10)
dimnames(x) <- list(letters[1:10], LETTERS[1:2])
input <- list(
  x, array(1:10), array(numeric(0L))
)
expect_error(
  bind_mat(input, 2L, name_deparse = NA),
  pattern = "`name_deparse` must be `TRUE` or `FALSE`",
  fixed = TRUE
)
expect_error(
  bind_mat(input, 2L, name_deparse = c(TRUE, FALSE)),
  pattern = "`name_deparse` must be `TRUE` or `FALSE`",
  fixed = TRUE
)
expect_error(
  bind_mat(input, 2L, comnames_from = 1:10),
  pattern = "`comnames_from` must be an integer scalar or `NULL`"
)
expect_error(
  bind_mat(input, 2L, comnames_from = NA_integer_),
  pattern = "`comnames_from` cannot be `NA`"
)
expect_error(
  bind_mat(input, 2L, comnames_from = 0L),
  pattern = "`comnames_from` out of bounds"
)
expect_error(
  bind_mat(input, 2L, comnames_from = -1),
  pattern = "`comnames_from` out of bounds"
)
expect_error(
  bind_mat(input, 2L, comnames_from = 4L),
  pattern = "`comnames_from` out of bounds"
)
expect_silent(
  bind_mat(input, 2L, comnames_from = 3L)
)
enumerate <- enumerate + 8L

