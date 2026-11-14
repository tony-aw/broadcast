
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
  bc.i(10 * 3e9, 8* 3e9, "gcd") |> drop(),
  2* 3e9
)
expect_equal(
  bc.i(-10* 3e9, 8* 3e9, "gcd") |> drop(),
  2* 3e9
)
expect_equal(
  bc.i(10* 3e9, -8* 3e9, "gcd") |> drop(),
  2* 3e9
)

x <- sample(1:100, 10) * 3e9
y <- sample(1:100, 10) * 3e9
expect_equal(
  bc.i(x, y, "gcd"),
  gcd(x, y)
)
enumerate <- enumerate + 4L



# gcd with standard known numbers ====
x <- c(48, 56) |> as.double()
y <- c(18, 98) |> as.double()
expect_equal(
  bc.i(x, y, "gcd"), c(6, 14)
)
enumerate <- enumerate + 1L



# gcd with coprimes ====
x <- c(17, 13) |> as.double()
y <- c(14, 25) |> as.double()
expect_equal(
  bc.i(x, y, "gcd"), c(1L, 1L)
)
enumerate <- enumerate + 1L




# gcd with identical numbers returns their absolute values ====
ids <- c(-100:-1, 1:100) |> as.double()
expect_equal(
  bc.i(ids, ids, "gcd"), abs(ids)
)
enumerate <- enumerate +1L



# gcd with zero ====
expect_equal(
  bc.i(0.1, 0.1, "gcd") |> drop(),
  NA_real_
)
samp <- sample(1:100, 10) * c(-1, 1)
for(i in samp) {
  expect_equal(
    bc.i(i, 0.1, "gcd") |> drop(),
    abs(i)
  ) |> errorfun()
  expect_equal(
    bc.i(0.1, i, "gcd") |> drop(),
    abs(i)
  ) |> errorfun()
  enumerate <- enumerate + 2L
}


# gcd with NAs and Infs ====

x.data <- list(
  NA,
  NA_integer_,
  c(Inf, -Inf, NA, NaN)
)
y.data <- list(
  NA,
  NA_integer_,
  c(Inf, -Inf, NA, NaN)
)
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- array(x.data[[i]])
    y <- array(y.data[[j]])
    out <- bc.i(x, y, "gcd")
    expect <- rep(NA_real_, length(out)) |> array()
    expect_equal(
      expect, out
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}



# gcd with datatypes WITHOUT zero(0) ====

x.data <- list(
  sample(1:100, 10),
  sample(1:100, 10) |> as.double()
)
y.data <- list(
  sample(1:100, 10),
  sample(1:100, 10) |> as.double()
)
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- array(x.data[[i]])
    y <- array(y.data[[j]])
    out <- bc.i(x, y, "gcd")
    expect <- gcd(as.integer(x), as.integer(y)) |> as.array()
    expect_equal(
      expect, out
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}

