
x <- array(1:24, 2:4)
ndim(x)

x <- list(
  array(1:10, 10),
  array(1:10, c(2, 5)),
  array(c(letters, NA), c(3,3,3))
)
lst.ndim(x)

x <- list(
  1:10,
  array(1:10, 10),
  matrix(1:10, 2, 5),
  array(c(letters, NA), c(3,3,3))
)
dim(x) <- c(2,2)
dimnames(x) <- list(c("a", "b"), c("x", "y"))
lst.ndim(x)


