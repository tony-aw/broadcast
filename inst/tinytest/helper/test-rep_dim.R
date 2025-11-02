
enumerate <- 0L

x <- matrix(1:9, 3,3)
colnames(x) <- LETTERS[1:3]
rownames(x) <- letters[1:3]
print(x)

expect_equal(
  x[c(1:3, 1:3), c(1:3, 1:3)],
  rep_dim(x, c(6,6))
)

expected <- array(NA, c(3,3,2))
expected[1:3, 1:3, ] <- x
colnames(expected) <- LETTERS[1:3]
rownames(expected) <- letters[1:3]
expect_equal(
  rep_dim(x, c(3,3,2)),
  expected
)

expect_error(
  rep_dim(1:10, c(10, 1)),
  pattern = "`x` must be an array"
)

expect_error(
  rep_dim(x, c(2,2)),
  pattern = "reduced dimensions not allowed"
)

expect_error(
  rep_dim(x, c(4, 4)),
  pattern = "fractional recycling not allowed"
)

enumerate <- enumerate + 5L

