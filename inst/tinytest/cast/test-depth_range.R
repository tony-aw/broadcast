

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist
.rcpp_clone <- broadcast:::.rcpp_clone



# consistency check ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  data.frame(letters)
)

out <- depth_range(x)
out2 <- depth_range(x)

expect_equal(
  out, out2
)



# no depth ====
x <- as.list(1:10)
expect_equal(
  depth_range(x),
  c(1L, 1L)
)
enumerate <- enumerate + 1L



# unit list ====
x <- list(list(list(list(NULL))))
expect_equal(
  depth_range(x),
  c(4L, 4L)
)

x <- list(list(list(list(data.frame(letters, LETTERS)))))
expect_equal(
  depth_range(x),
  c(4L, 4L)
)

x <- list(list(list(list(data.frame(letters)))))
expect_equal(
  depth_range(x, recurse_classed = TRUE),
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
  depth_range(x),
  c(1L, 5L)
)
expect_equal(
  depth_range(x, recurse_classed = TRUE),
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
  depth_range(x),
  c(1L, 5L)
)
expect_equal(
  depth_range(x, recurse_classed = TRUE),
  c(2L, 5L)
)

enumerate <- enumerate + 2L


# limit depth ====

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  depth_range(x, maxdepth = 16L),
  c(16L, 16L)
)

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  depth_range(x, maxdepth = 15L),
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

out <- depth_range(x)
out2 <- depth_range(x)

expect_equal(
  out, out2
)
expect_equal(
  x, y
)

enumerate <- enumerate + 2L



# errors ====
expect_error(
  depth_range(1:10)
)
expect_error(
  depth_range(as.list(1:10), NA),
  pattern = "`maxdepth` must be a single integer >= 1"
)
expect_error(
  depth_range(as.list(1:10), NA_integer_),
  pattern = "`maxdepth` must be a single integer >= 1"
)
expect_error(
  depth_range(as.list(1:10), 1:10),
  pattern = "`maxdepth` must be a single integer >= 1"
)
expect_error(
  depth_range(as.list(1:10), recurse_classed = NA),
  pattern = "`recurse_classed` must be `TRUE` or `FALSE`"
)
expect_error(
  depth_range(as.list(1:10), recurse_classed = c(TRUE, FALSE)),
  pattern = "`recurse_classed` must be `TRUE` or `FALSE`"
)
expect_error(
  depth_range(data.frame(letters, LETTERS)),
  pattern = "if `recurse_classed` is `FALSE`, `x` cannot be a classed list"
)
enumerate <- enumerate + 6L

