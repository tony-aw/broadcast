
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.C_determine_dimmode <- broadcast:::.C_determine_dimmode

# vector mode, scalars ====
expect_equal(
  .C_determine_dimmode(NULL, NULL, 1L, 1L),
  1L
) 
expect_equal(
  .C_determine_dimmode(1L, 1L, 1L, 1L),
  1L
)
enumerate <- enumerate + 2L


# vector mode, vectors ====
expect_equal(
  .C_determine_dimmode(NULL, NULL, 10L, 10L),
  1L
) 
expect_equal(
  .C_determine_dimmode(10L, 10L, 10L, 10L),
  1L
)
enumerate <- enumerate + 2L


# vector mode, vector X scalar ====
expect_equal(
  .C_determine_dimmode(10L, 1L, 10L, 1L),
  1L
)
expect_equal(
  .C_determine_dimmode(1L, 10L, 1L, 10L),
  1L
)
enumerate <- enumerate + 2L


# vector mode, array x scalar ====
expect_equal(
  .C_determine_dimmode(c(10L, 10L), c(1L, 1L), 100L, 1L),
  1L
)
expect_equal(
  .C_determine_dimmode(c(1L, 1L), c(10L, 10L), 1L, 100L),
  1L
)
enumerate <- enumerate + 2L



# vector mode, arrays of equal dimensions ====
x <- y <- array(prod(5:3), 5:3)
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  1L
)
enumerate <- enumerate + 1L


# orthovector mode ====
expect_equal(
  .C_determine_dimmode(c(10L, 1L), c(1L, 10L), 10L, 10L),
  2L
)
expect_equal(
  .C_determine_dimmode(c(1L, 10L), c(10L, 1L), 10L, 10L),
  2L
)
enumerate <- enumerate + 2L


# big2vector mode, matrix x vector (i.e. pre/post-ed vector) ====
x <- matrix(1:20, c(5, 4))
y <- array(1:5, c(5, 1))
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  3L
)
expect_equal(
  .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
  3L
)
x <- matrix(1:20, c(5, 4))
y <- array(1:5, c(1, 4))
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  3L
)
expect_equal(
  .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
  3L
)
enumerate <- enumerate + 4L



# big2vector mode, 3d x vector (i.e. sandwiched vector) ====
x <- array(1:20, 5:3)
y <- array(1:5, c(1, 4, 1))
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  3L
)
expect_equal(
  .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
  3L
)
enumerate <- enumerate + 2L


# ortho-sandwhich mode ====
for(iSample in 1:10) {
  n1 <- sample(2:10, 1L)
  n2 <- sample(2:10, 1L)
  n3 <- sample(2:10, 1L)
  x <- array(1:20, c(n1, 1L, n3))
  y <- array(1:5, c(1L, n2, 1L))
  expect_equal(
    .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
    4L
  ) |> errorfun()
  expect_equal(
    .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
    4L
  ) |> errorfun()
  enumerate <- enumerate + 2L
}


# general, non-sandwiched big2vector ===
x <- array(1:20, 5:3)
y <- array(1:5, c(1, 1, 3))
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  5L
)
expect_equal(
  .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
  5L
)

x <- array(1:20, 5:3)
y <- array(1:5, c(5, 1, 1))
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  5L
)
expect_equal(
  .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
  5L
)
enumerate <- enumerate + 4L


# general ====
x <- array(1:27, c(3,3,3))
y <- array(1:5, c(3, 1, 3))
expect_equal(
  .C_determine_dimmode(dim(x), dim(y), length(x), length(y)),
  5L
)
expect_equal(
  .C_determine_dimmode(dim(y), dim(x), length(y), length(x)),
  5L
)
enumerate <- enumerate + 2L


