
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
.return_missing <- broadcast:::.return_missing
types <- c("logical", "integer", "int53") # remember that Debian is stupid ...




# gcd simple ====
gcd <- function(x,y) { # straight-forward definition of gcd, for testing
  r <- x %% y
  return(ifelse(r, gcd(y, r), y))
}

expect_equal(
  bc.i(10L, 8L, "gcd") |> drop(),
  2L
)
expect_equal(
  bc.i(-10L, 8L, "gcd") |> drop(),
  2L
)
expect_equal(
  bc.i(10L, -8L, "gcd") |> drop(),
  2L
)

x <- sample(1:100, 10)
y <- sample(1:100, 10)
expect_equal(
  bc.i(x, y, "gcd"),
  gcd(x, y)
)
enumerate <- enumerate + 4L



# gcd with standard known numbers ====
x <- c(48, 56) |> as.integer()
y <- c(18, 98) |> as.integer()
expect_equal(
  bc.i(x, y, "gcd"), c(6, 14)
)
enumerate <- enumerate + 1L


# gcd with coprimes ====
x <- c(17, 13) |> as.integer()
y <- c(14, 25) |> as.integer()
expect_equal(
  bc.i(x, y, "gcd"), c(1L, 1L)
)
enumerate <- enumerate + 1L


# gcd with identical numbers returns their absolute values ====
ids <- c(-100:-1, 1:100) |> as.integer()
expect_equal(
  bc.i(ids, ids, "gcd"), abs(ids)
)
enumerate <- enumerate +1L




# gcd with zero ====
expect_equal(
  bc.i(0L, 0L, "gcd") |> drop(),
  NA_integer_
)
samp <- sample(1:100, 10L) * c(-1L, 1L)
for(i in samp) {
  expect_equal(
    bc.i(i, 0L, "gcd") |> drop(),
    abs(i)
  ) |> errorfun()
  expect_equal(
    bc.i(0L, i, "gcd") |> drop(),
    abs(i)
  ) |> errorfun()
  enumerate <- enumerate + 2L
}


# gcd with NAs ====

x.data <- list(
  NA,
  NA_integer_
)
y.data <- list(
  NA,
  NA_integer_
)
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- array(x.data[[i]])
    y <- array(y.data[[j]])
    out <- bc.i(x, y, "gcd")
    expect <- rep(NA_integer_, length(out)) |> array()
    expect_equal(
      expect, out
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}



# gcd with datatypes WITHOUT 0/FALSE ====

x.data <- list(
  sample(1:100, 10)
)
y.data <- list(
  sample(1:100, 10)
)
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- array(x.data[[i]])
    y <- array(y.data[[j]])
    out <- bc.i(x, y, "gcd")
    expect <- gcd(as_int(x), as_int(y)) |> as.array()
    expect_equal(
      expect, out
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}


# gcd with dimensions ====

bc.fun <- \(x, y) bc.i(x, y, "gcd")

res <- .test_binary(bc.fun, gcd, "uint", "uint")
expect_equal(
  res$expected, res$out
)

