
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

x <- c(
  lapply(1:3, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:3, \(x)sample(1:10)),
  lapply(1:3, \(x)rnorm(10)),
  lapply(1:3, \(x)sample(letters))
)
x <- matrix(x, 4, 3, byrow = TRUE)
dimnames(x) <- list(letters[1:4], LETTERS[1:3])
print(x)

y <- matrix(1:12, 4, 3)
print(y)


# tests ====
input <- list(x = x, y = y)
expect_equal( # binds on new dimension before first
  bind_array(input, along = 0L),
  bind_array(input, along = 3L, TRUE)
)

expect_equal( # binds on first dimension (i.e. rows)
  bind_array(input, along = 1L),
  bind_array(input, along = 2L, TRUE)
)

expect_equal( # binds on last dimension (i.e. columns)
  bind_array(input, along = 2L),
  bind_array(input, along = 1L, TRUE)
)

expect_equal( # bind on new dimension after last
  bind_array(input, along = 3L),
  bind_array(input, along = 0L, TRUE)
)

enumerate <- enumerate + 4L
