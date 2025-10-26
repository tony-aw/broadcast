
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# no names ====
x <- rep(list(1:11), 10)
expect_equal(
  cast_shallow2atomic(x),
  unlist(x)
)


# empty names ====
x <- rep(list(setNames(1:11, rep("", 11))), 10)
names(x) <- rep("", length(x))
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

x <- rep(list(setNames(1:11, rep("", 11))), 10)
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

x <- list(setNames(1:10, rep("", 10)), 1:11)
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

x <- rep(list(1:11), 10)
names(x) <- rep("", length(x))
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

enumerate <- enumerate + 5L


# partially named ====
# only some content named:
x <- list(
  setNames(1:11, letters[1:11]), 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
)
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

# only some elements named:
x <- list( a = 1:11, 1:10, 1:9)
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

# cross-named:
x <- list(a = 1:11, setNames(1:10, letters[1:10]))
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

# simultaneously named:
x <- list(a = setNames(1:10, letters[1:10]), b = 1:20)
expect_equal(
  cast_shallow2atomic(x),
  unlist(x, use.names = TRUE)
)

enumerate <- enumerate + 4L


# fully named ====
x <- rep(list(setNames(1:11, letters[1:11])))
names(x) <- sample(letters, length(x))
expect_equal(
  cast_shallow2atomic(x),
  unlist(x)
)

