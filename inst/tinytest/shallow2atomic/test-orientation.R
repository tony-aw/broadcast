
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# unit list ===
x <- list(1:11)
expect_equal(
  cast_shallow2atomic(x, 1),
  array(unlist(x), c(lengths(x), 1))
)
expect_equal(
  cast_shallow2atomic(x, -1),
  array(unlist(x), c(1, lengths(x)))
)

enumerate <- enumerate + 2L


# vector ====
x <- list(1:11, 1:11)
expect_equal(
  cast_shallow2atomic(x, 1),
  simplify2array(x)
)
expect_equal(
  cast_shallow2atomic(x, -1),
  simplify2array(x) |> t()
)

enumerate <- enumerate + 2L


# 1d  ====
x <- array(list(1:11), 11)
expect_equal(
  cast_shallow2atomic(x, 1),
  simplify2array(x)
)
expect_equal(
  cast_shallow2atomic(x, -1),
  simplify2array(x) |> t()
)

enumerate <- enumerate + 2L


# matrix ====
x <- list(
  1:11, 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
) |> rev()
dim(x) <- c(3, 4)

expected <- lapply(x, \(x){
  length(x) <- 11
  x
})
expected <- do.call(cbind, expected)
dim(expected) <- c(11, 3, 4)

expect_equal(
  cast_shallow2atomic(x, 1),
  expected
)

expected <- lapply(x, \(x){
  length(x) <- 11
  x
})
expected <- do.call(rbind, expected)
dim(expected) <- c(3, 4, 11)

expect_equal(
  cast_shallow2atomic(x, -1),
  expected
)

enumerate <- enumerate + 2L



