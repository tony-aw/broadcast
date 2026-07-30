

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist
.rcpp_clone <- broadcast:::.rcpp_clone
.rcpp_depth_range <- broadcast:::.rcpp_depth_range



# consistency check ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  data.frame(letters)
)

out <- .rcpp_depth_range(x, 32L, FALSE)
out2 <- .rcpp_depth_range(x, 32L, FALSE)

expect_equal(
  out, out2
)



# no depth ====
x <- as.list(1:10)
expect_equal(
  .rcpp_depth_range(x, 32L, FALSE),
  c(1L, 1L)
)
enumerate <- enumerate + 1L



# unit list ====
x <- list(list(list(list(NULL))))
expect_equal(
  .rcpp_depth_range(x, 32L, FALSE),
  c(4L, 4L)
)

x <- list(list(list(list(data.frame(letters, LETTERS)))))
expect_equal(
  .rcpp_depth_range(x, 32L, FALSE),
  c(4L, 4L)
)

x <- list(list(list(list(data.frame(letters)))))
expect_equal(
  .rcpp_depth_range(x, 32L, recurse_all = TRUE),
  c(5L, 5L)
)

enumerate <- enumerate + 3L


# recursive vector ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  list(list(~ hello)),
  data.frame(letters)
)
expect_equal(
  .rcpp_depth_range(x, 32L, FALSE),
  c(1L, 5L)
)
expect_equal(
  .rcpp_depth_range(x, 32L, recurse_all = TRUE),
  c(2L, 5L)
)

enumerate <- enumerate + 2L


# recursive matrix ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  list(list(~ hello)),
  data.frame(letters)
)
dim(x) <- c(3, 2)
expect_equal(
  .rcpp_depth_range(x, 32L, FALSE),
  c(1L, 5L)
)
expect_equal(
  .rcpp_depth_range(x, 32L, recurse_all = TRUE),
  c(2L, 5L)
)

enumerate <- enumerate + 2L


# limit depth ====

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  .rcpp_depth_range(x, 16L, FALSE),
  c(16L, 16L)
)

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  .rcpp_depth_range(x, 15L, FALSE),
  c(15L, 15L)
)

enumerate <- enumerate + 2L


# pass-by-reference safety checks ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  data.frame(letters)
)
y <- .rcpp_clone(x)

out <- .rcpp_depth_range(x, 32L, FALSE)
out2 <- .rcpp_depth_range(x, 32L, FALSE)

expect_equal(
  out, out2
)
expect_equal(
  x, y
)

enumerate <- enumerate + 2L


