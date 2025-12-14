
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 3:
  out <- lapply(1:n, \(n)sample(c(1, 3), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}

# dimnames ====
x <- array(1:10)
y <- array(1:10, c(1, 10))
test <- bc.d(x, y, ">")
dimnames(test) <- list(letters[1:10], LETTERS[1:10])

expect_equal(
  bc_ifelse(test, x, y) |> dimnames(),
  dimnames(test)
)

dim(test) <- length(test)
dimnames(test) <- list(sample(letters, length(test), TRUE))

expect_equal(
  bc_ifelse(test, x, y) |> dimnames(),
  NULL
)

enumerate <- enumerate + 2L



# names ====
x <- 1:10
y <- 1:10
test <- bc.d(x, y, ">")
names(test) <- sample(letters[1:10])

expect_equal(
  bc_ifelse(test, x, y) |> names(),
  names(test)
)

dim(test) <- length(test)
names(test) <- sample(letters[1:10])

expect_equal(
  bc_ifelse(test, x, y) |> names(),
  names(test)
)

expect_equal(
  bc_ifelse(test, as.array(x), y) |> names(),
  names(test)
)

expect_equal(
  bc_ifelse(test, x, as.array(y)) |> names(),
  names(test)
)


enumerate <- enumerate + 4L


# broadcaster ====
x <- 1:10
y <- 1:10
test <- bc.d(x, y, ">")

expect_equal(
  bc_ifelse(test, x, y) |> broadcaster(),
  broadcaster(test)
)

broadcaster(test) <- TRUE
expect_equal(
  bc_ifelse(test, x, y) |> broadcaster(),
  broadcaster(test)
)

broadcaster(test) <- FALSE
class(test) <- "mutatomic"
expect_equal(
  bc_ifelse(test, x, y) |> oldClass(),
  oldClass(test)
)

broadcaster(test) <- TRUE
expect_equal(
  bc_ifelse(test, x, y) |> oldClass(),
  oldClass(test)
)

enumerate <- enumerate + 4L


# comment ====
x <- 1:10
y <- 1:10
test <- bc.d(x, y, ">")

expect_equal(
  bc_ifelse(test, x, y) |> comment(),
  comment(test)
)

comment(test) <- "test comment"
expect_equal(
  bc_ifelse(test, x, y) |> comment(),
  comment(test)
)

enumerate <- enumerate + 2L



# broadcaster, zerolen ====
x <- 1:10
y <- 1:10
test <- logical(0L)

expect_equal(
  bc_ifelse(test, x, y) |> broadcaster(),
  broadcaster(test)
)

broadcaster(test) <- TRUE
expect_equal(
  bc_ifelse(test, x, y) |> broadcaster(),
  broadcaster(test)
)

enumerate <- enumerate + 2L


# comment, zerolen ====
x <- 1:10
y <- 1:10
test <- logical(0L)

expect_equal(
  bc_ifelse(test, x, y) |> comment(),
  comment(test)
)

comment(test) <- "test comment"
expect_equal(
  bc_ifelse(test, x, y) |> comment(),
  comment(test)
)

enumerate <- enumerate + 2L


