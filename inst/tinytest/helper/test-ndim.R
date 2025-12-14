# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops



# ndim() ====

expect_equal(
  ndim(1:10),
  0L
)
expect_equal(
  ndim(NULL),
  0L
)
expect_equal(
  ndim(integer(0L)),
  0L
)
expect_equal(
  ndim(list()),
  0L
)
expect_equal(
  ndim(array(1:24, 2:4)),
  3L
)
enumerate <- enumerate + 5L


# lst.ndim() ====

x <- list(
  array(c(TRUE, FALSE, NA), c(10)),
  array(1:10, c(2, 5)),
  array(rnorm(10), c(2, 5)),
  array(rnorm(10) + rnorm(10) * -1i, c(4, 4, 4, 4)),
  array(c(letters, NA), c(3,3,3)),
  array(lapply(1:10, \(i)letters[i])),
  array(list(), c(0L)),
  NULL
)
dim(x) <- c(4, 2)
dimnames(x) <- list(letters[1:4], letters[1:2])
names(x) <- letters[1:8]

out <- c(1L, 2L, 2L, 4L, 3L, 1L, 1L, 0L)
attributes(out) <- attributes(x)
expect_equal(
  lst.ndim(x),
  out
)

broadcaster(x) <- TRUE
attributes(out) <- attributes(x)
expect_equal(
  lst.ndim(x),
  out
)

expect_equal(
  lst.ndim(list(1:10)),
  0L
)
expect_equal(
  lst.ndim(list(NULL)),
  0L
)
expect_equal(
  lst.ndim(list(integer(0L))),
  0L
)
expect_equal(
  lst.ndim(list()),
  integer(0L)
)

enumerate <- enumerate + 7L


