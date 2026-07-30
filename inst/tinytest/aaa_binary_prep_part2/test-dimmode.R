
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


determine_dimmode <- broadcast:::.rcpp_virt_dimmode

# vector mode, scalars ====
expect_equal(
  determine_dimmode(NULL, NULL, 1L, 1L, 0L),
  1L
) 
expect_equal(
  determine_dimmode(1L, 1L, 1L, 1L, 1L),
  1L
)
enumerate <- enumerate + 2L


# vector mode, vectors ====
expect_equal(
  determine_dimmode(NULL, NULL, 10L, 10L, 0L),
  1L
) 
expect_equal(
  determine_dimmode(10L, 10L, 10L, 10L, 1L),
  1L
)
enumerate <- enumerate + 2L


# vector mode, vector X scalar ====
expect_equal(
  determine_dimmode(10L, 1L, 10L, 1L, 1L),
  1L
)
expect_equal(
  determine_dimmode(1L, 10L, 1L, 10L, 1L),
  1L
)
enumerate <- enumerate + 2L


# vector mode, array x scalar ====
expect_equal(
  determine_dimmode(c(10L, 10L), c(1L, 1L), 100L, 1L, 2L),
  1L
)
expect_equal(
  determine_dimmode(c(1L, 1L), c(10L, 10L), 1L, 100L, 2L),
  1L
)
enumerate <- enumerate + 2L



# vector mode, arrays of equal dimensions ====
x <- y <- array(prod(5:3), 5:3)
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), ndim(x)),
  1L
)
enumerate <- enumerate + 1L


# orthovector mode ====
expect_equal(
  determine_dimmode(c(10L, 1L), c(1L, 10L), 10L, 10L, 2L),
  2L
)
expect_equal(
  determine_dimmode(c(1L, 10L), c(10L, 1L), 10L, 10L, 2L),
  2L
)
enumerate <- enumerate + 2L


# big2vector mode, matrix x vector (i.e. pre/post-ed vector) ====
x <- matrix(1:20, c(5, 4))
y <- array(1:5, c(5, 1))
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), 2L),
  3L
)
expect_equal(
  determine_dimmode(dim(y), dim(x), length(y), length(x), 2L),
  3L
)
x <- matrix(1:20, c(5, 4))
y <- array(1:5, c(1, 4))
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), 2L),
  3L
)
expect_equal(
  determine_dimmode(dim(y), dim(x), length(y), length(x), 2L),
  3L
)
enumerate <- enumerate + 4L



# big2vector mode, 3d x vector (i.e. sandwiched vector) ====
x <- array(1:20, 5:3)
y <- array(1:5, c(1, 4, 1))
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), 3L),
  3L
)
expect_equal(
  determine_dimmode(dim(y), dim(x), length(y), length(x), 3L),
  3L
)
enumerate <- enumerate + 2L



# orthogonal sandwhich mode ====
for(iSample in 1:10) {
  n1 <- sample(2:10, 1L)
  n2 <- sample(2:10, 1L)
  n3 <- sample(2:10, 1L)
  x <- array(1:20, c(n1, 1L, n3))
  y <- array(1:5, c(1L, n2, 1L))
  expect_equal(
    determine_dimmode(dim(x), dim(y), length(x), length(y), 3L),
    4L
  ) |> errorfun()
  expect_equal(
    determine_dimmode(dim(y), dim(x), length(y), length(x), 3L),
    4L
  ) |> errorfun()
  enumerate <- enumerate + 2L
}


# general, non-sandwiched big2vector ===
x <- array(1:20, 5:3)
y <- array(1:5, c(1, 1, 3))
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), 3L),
  5L
)
expect_equal(
  determine_dimmode(dim(y), dim(x), length(y), length(x), 3L),
  5L
)

x <- array(1:20, 5:3)
y <- array(1:5, c(5, 1, 1))
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), 3L),
  5L
)
expect_equal(
  determine_dimmode(dim(y), dim(x), length(y), length(x), 3L),
  5L
)
enumerate <- enumerate + 4L


# general ====
x <- array(1:27, c(3,3,3))
y <- array(1:5, c(3, 1, 3))
expect_equal(
  determine_dimmode(dim(x), dim(y), length(x), length(y), 3L),
  5L
)
expect_equal(
  determine_dimmode(dim(y), dim(x), length(y), length(x), 3L),
  5L
)
enumerate <- enumerate + 2L


# general, full orthogonal ====

for(i in 4:15) {
  n <- ceiling(10000^(1/i)) |> as.integer()
  x.dim <- rep(c(1L, n), 8L)[1:i]
  y.dim <- rep(c(n, 1L), 8L)[1:i]
  x.len <- prod(x.dim)
  y.len <- prod(y.dim)
  
  expect_equal(
    determine_dimmode(x.dim, y.dim, x.len, y.len, i),
    5L
  ) |> errorfun()
  
  expect_equal(
    determine_dimmode(y.dim, x.dim, y.len, x.len, i),
    5L
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}



