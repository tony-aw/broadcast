
enumerate <- 0L

x <- array(1:27, c(3,3,3), list(letters[1:3], NULL, letters[4:6]))
y <- array(1:27, c(3,3,3), list(NULL, NULL, NULL))

# comnames_from == 1L ====
input <- list(x, y)

# along rows:
expected.dimnames <- list(
  c(letters[1:3], rep("", 3)),
  NULL,
  letters[4:6]
)
expect_equal(
  bind_array(input, 1) |> dimnames(),
  expected.dimnames
)

# along columns:
expected.dimnames <- list(
  c(letters[1:3]),
  NULL,
  letters[4:6]
)
expect_equal(
  bind_array(input, 2) |> dimnames(),
  expected.dimnames
)


# along layers:
expected.dimnames <- list(
  letters[1:3],
  NULL,
  c(letters[4:6], rep("", 3))
)
expect_equal(
  bind_array(input, 3) |> dimnames(),
  expected.dimnames
)

enumerate <- enumerate + 3L



# comnames_from == length(input) ====
input <- list(y, x)

# along rows:
expected.dimnames <- list(
  c(rep("", 3), letters[1:3]),
  NULL,
  letters[4:6]
)
expect_equal(
  bind_array(input, 1, comnames_from = 2L) |> dimnames(),
  expected.dimnames
)

# along columns:
expected.dimnames <- list(
  c(letters[1:3]),
  NULL,
  letters[4:6]
)
expect_equal(
  bind_array(input, 2, comnames_from = 2L) |> dimnames(),
  expected.dimnames
)


# along layers:
expected.dimnames <- list(
  letters[1:3],
  NULL,
  c(rep("", 3), letters[4:6])
)
expect_equal(
  bind_array(input, 3, comnames_from = 2L) |> dimnames(),
  expected.dimnames
)

enumerate <- enumerate + 3L


