
.C_make_outdim <- broadcast:::.C_make_outdim

# test x & y are NULL
x <- NULL
y <- NULL
expect_equal(
  .C_make_outdim(x, y),
  NULL
)


# test NULL by scalar
x <- sample(1:10, 1L)
y <- NULL
expect_equal(
  .C_make_outdim(x, y),
  x
)
expect_equal(
  .C_make_outdim(y, x),
  x
)

# test NULL by vector
x <- NULL
y <- sample(1:10)
expect_equal(
  .C_make_outdim(x, y),
  y
)
expect_equal(
  .C_make_outdim(y, x),
  y
)


# test x & y are equal scalars
scalar <- sample(1:10, 1L)
x <- scalar
y <- scalar
expect_equal(
  .C_make_outdim(x, y),
  scalar
)
expect_equal(
  .C_make_outdim(y, x),
  scalar
)



# test x & y are unequal but comformable scalars
x <- 10L
y <- 1L
expect_equal(
  .C_make_outdim(x, y),
  max(x, y)
)
expect_equal(
  .C_make_outdim(y, x),
  max(x, y)
)



# test equal lens
x <- sample(1:10)
y <- sample(1:10)
expect_equal(
  .C_make_outdim(x, y),
  pmax(x, y)
)


# test exceed max len
x <- c(2^31 - 2, 1) |> as.integer()
y <- c(1, 2^31 - 2) |> as.integer()
expect_error(
  .C_make_outdim(x, y),
  pattern = "broadcasting will exceed maximum size"
)
x <- c(165141L, 1L, 165141L)
y <- c(1L, 165141L, 1L)
expect_error(
  .C_make_outdim(x, y),
  pattern = "broadcasting will exceed maximum size"
)

# large array test
x <- sample(1:3L, 16L, TRUE)
y <- sample(1:3L, 16L, TRUE)
expect_equal(
  .C_make_outdim(x, y),
  pmax(x, y)
)

enumerate <- 11L

