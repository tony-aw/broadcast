
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
  bind_dt(list(), 1L),
  pattern = "`input` must be a list with at least 2 elements"
)
expect_error(
  bind_dt(list(cbind(1:10, 1:10)), 1L),
  pattern = "`input` must be a list with at least 2 elements"
)

enumerate <- enumerate + 2L



# arg along errors ====
input <- list(
  array(1:10), array(1:10)
)
expect_error(
  bind_dt(input, 1:10),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)
expect_error(
  bind_dt(input, "a"),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)
expect_error(
  bind_dt(input, -1),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)
expect_error(
  bind_dt(input, 3, TRUE),
  pattern = "`along` must be the integer scalar 1 or 2",
  fixed = TRUE
)

enumerate <- enumerate + 4L


