
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

int53max <- 2^53 - 1
int53min <- -1 * int53max
int32max <- as.integer(2^31 - 1)
int32min <- as.integer(-1 * int32max)
ints <- list(
  Inf, int53max + 1, int53max, int32max, 0.5,
  0L, NA, NaN,
  -0.5, int32min, int53min, int53min - 1, -Inf
)


# plus/minus ====
x <- as.integer(2^31 - 10)
y <- as.integer(2^31 - 10)

expect_error(
  bc.d(x, y, "+"),
  pattern = "integer overflow"
)

expect_error(
  bc.d(-x, y, "-"),
  pattern = "integer overflow"
)

expect_equal(
  bc.d(x, y, "-"),
  0L
)

x <- as.integer(2^(0:30) - 1)
expect_equal(
  bc.d(x, x, "+"),
  x + x
)

enumerate <- enumerate + 5L


# mdp ====
x <- as.integer(2^16 - 2)
y <- as.integer(2^16 - 2)

expect_equal(
  bc.d(x, y, "*"),
  as_dbl(x) * as_dbl(y)
)

expect_equal(
  bc.d(x, y, "/"),
  as_dbl(x) / as_dbl(y)
)

expect_equal(
  bc.d(x, 2L, "^"),
  as_dbl(x) ^ 2L
)

expect_equal(
  bc.d(2L, x, "^"),
  2L ^ x
)

enumerate <- enumerate + 5L




# idiv ====
for(i in ints) {
  for(j in ints) {
    i2 <- round2int53(i)
    j2 <- round2int53(j)
    expect_equal(
      bc.i(i, j, "%/%"),
      i2 %/% j2
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# mod ====
for(i in ints) {
  for(j in ints) {
    i2 <- round2int53(i)
    j2 <- round2int53(j)
    expect_equal(
      bc.i(i, j, "%%"),
      i2 %% j2
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# gcd ====
gcd <- function(x,y) { # straight-forward definition of gcd, for testing
  x <- abs(x)
  y <- abs(y)
  r <- x %% y
  return(ifelse(r, gcd(y, r), y))
}
for(i in ints) {
  for(j in ints) {
    
    # print(i)
    # print(j)
    
    if(isTRUE(i > int53max || i < int53min)) {
      i2 <- NA_real_
    }
    else {
      i2 <- round2int53(abs(i))
    }
    if(isTRUE(j > int53max || j < int53min)) {
      j2 <- NA_real_
    }
    else {
      j2 <- round2int53(abs(j))
    }
    
    if(isTRUE(i2 == 0 && j2 == 0)) {
      expected <- NA_real_
    }
    else if(isTRUE(i2 == 0)) {
      expected <- j2
    }
    else if(isTRUE(j2 == 0)) {
      expected <- i2
    }
    else {
      expected <- gcd(i2, j2)
    }
    
    out <- bc.i(i, j, "gcd")
    if(is.na(expected) && is.na(out)) { # because ifelse() forces NA to be logcial
      expected <- as.double(expected)
      out <- as.double(out)
    }
    
    expect_equal(
      expected, out
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}
