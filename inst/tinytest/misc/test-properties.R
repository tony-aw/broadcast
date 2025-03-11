# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops


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

out <- c("logical", "integer", "double", "complex", "character", "list", "list", "NULL")
attributes(out) <- attributes(x)
expect_equal(
  lst.typeof(x),
  out
)

enumerate <- enumerate + 2L


