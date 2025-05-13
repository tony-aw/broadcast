x <- sample(11:21)
y <- sample(0:10)

expect_equal(
  as.raw(x + y),
  bc.raw(as.raw(x), as.raw(y), "+")
)

expect_equal(
  as.raw(x - y),
  bc.raw(as.raw(x), as.raw(y), "-")
)

expect_equal(
  as.raw(x * y),
  bc.raw(as.raw(x), as.raw(y), "*")
)
