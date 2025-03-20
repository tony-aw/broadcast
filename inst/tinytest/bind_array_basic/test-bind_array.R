
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# 1d ====
x <- array(sample(1:10))
y <- array(sample(1:10, 1L))
names(x) <- letters[1:10]
names(y) <- "a"

expect_equal(
  array(c(x, y), dimnames = list(c(letters[1:10], "a"))),
  bind_array(list(x, y), 1L)
)

expected <- rbind(x, y)
dimnames(expected) <- list(c("x", "y"), letters[1:10])
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 0L)
)

expected <- cbind(x, y)
dimnames(expected) <- list(letters[1:10], c("x", "y"))
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 2L)
)
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 0L, TRUE)
)

enumerate <- enumerate + 4L


# 2d ====
x <- matrix(sample(1:10), 5, 4, dimnames = list(letters[1:5], LETTERS[1:4]))
y <- array(1:4, c(1, 4), dimnames = list("a", letters[1:4]))

expected <- rbind(x, as.vector(y))
dimnames(expected) <- list(c(rownames(x), "a"), colnames(x))
expect_equal(
  expected,
  bind_array(list(x, y), 1L)
)

expected <- cbind(x, rep_dim(y, c(5, 4)))
dimnames(expected) <- list(rownames(x), c(colnames(x), colnames(y)))
expect_equal(
  expected,
  bind_array(list(x, y), 2L)
)

expected <- array(NA, c(2, 5, 4))
expected[1,,] <- x
expected[2,,] <- rep_dim(y, c(5, 4))
dimnames(expected) <- list(c("x", "y"), rownames(x), colnames(x))
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 0L)
)

expected <- array(NA, c(5, 4, 2))
expected[,,1] <- x
expected[,,2] <- rep_dim(y, c(5, 4))
dimnames(expected) <- list(rownames(x), colnames(x), c("x", "y"))
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 3L)
)
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 0L, TRUE)
)

enumerate <- enumerate + 5L



# 3d ====
x <- array(1:4, c(5, 1, 3), dimnames = list(LETTERS[1:5], "a", letters[1:3]))
y <- array(
  sample(1:27),
  5:3,
  dimnames = list(month.abb[1:5], month.name[1:4], rev(month.abb)[1:3])
)

expected <- array(NA, c(10, 4, 3))
expected[1:5, , ] <- rep_dim(x, 5:3)
expected[6:10, , ] <- y
dimnames(expected) <- list(
  c(rownames(x), rownames(y)), NULL, dimnames(x)[[3L]]
)
expect_equal(
  expected,
  bind_array(list(x, y), 1L)
)

expected <- array(NA, c(5, 5, 3))
expected[, 1, ] <- x
expected[, 2:5, ] <- y
dimnames(expected) <- list(
  rownames(x), c(colnames(x), colnames(y)), dimnames(x)[[3L]]
)
expect_equal(
  expected,
  bind_array(list(x, y), 2L)
)


expected <- array(NA, c(5, 4, 6))
expected[, , 1:3] <- rep_dim(x, 5:3)
expected[, , 4:6] <- y
dimnames(expected) <- list(
  rownames(x), NULL, c(dimnames(x)[[3L]], dimnames(y)[[3L]])
)
expect_equal(
  expected,
  bind_array(list(x, y), 3L)
)


expected <- array(NA, c(2, 5:3))
expected[1, , , ] <- rep_dim(x, 5:3)
expected[2, , , ] <- y
dimnames(expected) <- list(
  c("x", "y"), rownames(x), NULL, dimnames(x)[[3L]]
)
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 0L)
)

expected <- array(NA, c(5:3, 2))
expected[, , , 1] <- rep_dim(x, 5:3)
expected[, , , 2] <- y
dimnames(expected) <- list(
  rownames(x), NULL, dimnames(x)[[3L]], c("x", "y")
)
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 4L)
)
expect_equal(
  expected,
  bind_array(list(x = x, y = y), 0L, TRUE)
)
enumerate <- enumerate + 6L

