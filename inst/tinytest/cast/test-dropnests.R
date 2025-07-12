

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

out <- dropnests(x)
out2 <- dropnests(x)

expect_equal(
  out, out2
)



# no dropping ====
x <- as.list(1:10)
expect_equal(
  dropnests(x),
  x
)
x <- as.list(1:10)
dim(x) <- c(5, 2)
dimnames(x) <- list(letters[1:5], month.abb[1:2])
attr(x, "test") <- "test"
expect_equal(
  dropnests(x),
  x
)

x <- .rcpp_allocate_nestedlist(rep(1, 10), 1)
expect_equal(
  dropnests(x, maxdepth = 1L),
  x
)


enumerate <- enumerate + 3L



# unit list ====
x <- list(list(list(list(NULL))))
expect_equal(
  dropnests(x),
  list(NULL)
)

x <- list(list(list(list(data.frame(letters, LETTERS)))))
expect_equal(
  dropnests(x),
  list(data.frame(letters, LETTERS))
)

x <- list(list(list(list(data.frame(letters)))))
expect_equal(
  dropnests(x, recurse_classed = TRUE),
  list(letters)
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
names(x) <- letters[1:6]
attr(x, "test") <- "test"
expected <- list(
  1L,
  as.list(1:10),
  list(),
  NULL,
  ~ hello,
  data.frame(letters)
)
names(expected) <- names(x)
attr(expected, "test") <- "test"

expect_equal(
  dropnests(x),
  expected
)

expected <- list(
  1L,
  as.list(1:10),
  list(),
  NULL,
  ~ hello,
  letters
)
names(expected) <- names(x)
attr(expected, "test") <- "test"

expect_equal(
  dropnests(x, recurse_classed = TRUE),
  expected
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
dimnames(x) <- list(letters[1:3], month.abb[1:2])
attr(x, "test") <- "test"
expected <- list(
  1L,
  as.list(1:10),
  list(),
  NULL,
  ~ hello,
  data.frame(letters)
)
dim(expected) <- dim(x)
dimnames(expected) <- dimnames(x)
attr(expected, "test") <- "test"

expect_equal(
  dropnests(x),
  expected
)

expected <- list(
  1L,
  as.list(1:10),
  list(),
  NULL,
  ~ hello,
  letters
)
dim(expected) <- dim(x)
dimnames(expected) <- dimnames(x)
attr(expected, "test") <- "test"
expect_equal(
  dropnests(x, recurse_classed = TRUE),
  expected
)

enumerate <- enumerate + 2L


# limit depth ====

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  dropnests(x, maxdepth = 16L),
  .rcpp_allocate_nestedlist(rep(1, 5), 1)
)

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  dropnests(x, maxdepth = 15),
  .rcpp_allocate_nestedlist(rep(1, 6), 1)
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

out <- dropnests(x)
out2 <- dropnests(x)

expect_equal(
  out, out2
)
expect_equal(
  x, y
)

enumerate <- enumerate + 2L



# errors ====
expect_error(
  dropnests(1:10)
)
expect_error(
  dropnests(as.list(1:10), NA),
  pattern = "`maxdepth` must be a single integer >= 1"
)
expect_error(
  dropnests(as.list(1:10), NA_integer_),
  pattern = "`maxdepth` must be a single integer >= 1"
)
expect_error(
  dropnests(as.list(1:10), 1:10),
  pattern = "`maxdepth` must be a single integer >= 1"
)
expect_error(
  dropnests(as.list(1:10), recurse_classed = NA),
  pattern = "`recurse_classed` must be `TRUE` or `FALSE`"
)
expect_error(
  dropnests(as.list(1:10), recurse_classed = c(TRUE, FALSE)),
  pattern = "`recurse_classed` must be `TRUE` or `FALSE`"
)
expect_error(
  dropnests(data.frame(letters, LETTERS)),
  pattern = "if `recurse_classed` is `FALSE`, `x` cannot be a classed list"
)
enumerate <- enumerate + 6L

