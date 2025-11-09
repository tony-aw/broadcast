
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.set_dimnames <- broadcast:::.set_dimnames


# incorrect context checks ====
# vector not assigned dimnames:
x <- 1:10
expect_silent(
  .set_dimnames(x, letters[1:10])
)
expect_null(
  dimnames(x)
)
enumerate <- enumerate + 2L


# incorrect length checks ====

# empty list dimnames not assigned:
x <- array(1:27, c(3,3,3))
expect_silent(
  .set_dimnames(x, list())
)
.set_dimnames(x, list())
expect_null(
  dimnames(x)
)

# dimnames of incorrect length not assigned:
x <- array(1:27, c(3,3,3))
expect_silent(
  .set_dimnames(x, list(1:27))
)
.set_dimnames(x, list(1:27))
expect_null(
  dimnames(x)
)

# dimnames of incorrect lengths not assigned:
x <- array(1:27, c(3,3,3))
expect_silent(
  .set_dimnames(x, list(letters[1:4], letters[1:3], letters[1:4]))
)
.set_dimnames(x, list(letters[1:4], letters[1:3], letters[1:4]))
expect_null(
  dimnames(x)
)

enumerate <- enumerate + 6L


# incorrect type checks ====

# non-list & non-NULL dimnames not assigned:
x <- array(1:27, c(3,3,3))
expect_silent(
  .set_dimnames(x, sample(letters, 27, TRUE))
)
.set_dimnames(x, sample(letters, 27, TRUE))
expect_null(
  dimnames(x)
)

enumerate <- enumerate + 2L


# type conversion checks ====

# list of non-character dimnames automatically converted to character:
x <- array(1:27, c(3,3,3))
newdimnames <- list(1:3, 1:3, 1:3)
expect_silent(
  .set_dimnames(x, newdimnames)
)
.set_dimnames(x, newdimnames)
expect_equal(
  dimnames(x),
  lapply(newdimnames, as.character)
)


# removing dimnames with NULL BY REFERENCE ====
x.dimnames <- list(letters[1:3], letters[4:6], letters[7:9])
x <- y <- array(1:27, c(3,3,3), x.dimnames)
expect_equal(
  dimnames(x),
  x.dimnames
)
expect_equal(
  dimnames(y),
  x.dimnames
)
expect_silent(
  .set_dimnames(x, NULL)
)
.set_dimnames(x, NULL)
expect_null(
  dimnames(x)
)
expect_null(
  dimnames(y)
)
enumerate <- enumerate + 5L


# assigning dimnames with list of character vectors BY REFERENCE====
x <- y <- array(1:27, c(3,3,3))
expect_null(
  dimnames(x)
)
expect_null(
  dimnames(y)
)
x.dimnames <- list(letters[1:3], letters[4:6], letters[7:9])
expect_silent(
  .set_dimnames(x, x.dimnames)
)
.set_dimnames(x, x.dimnames)
expect_equal(
  dimnames(x),
  x.dimnames
)
expect_equal(
  dimnames(y),
  x.dimnames
)
enumerate <- enumerate + 5L



# PARTIALLY assigning dimnames with list of character vectors BY REFERENCE====
x <- y <- array(1:27, c(3,3,3))
expect_null(
  dimnames(x)
)
expect_null(
  dimnames(y)
)
x.dimnames <- list(letters[1:3], NULL, letters[7:9])
expect_silent(
  .set_dimnames(x, x.dimnames)
)
.set_dimnames(x, x.dimnames)
expect_equal(
  dimnames(x),
  x.dimnames
)
expect_equal(
  dimnames(y),
  x.dimnames
)
enumerate <- enumerate + 5L

rm(.set_dimnames)

