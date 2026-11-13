
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

# idiv ====
for(i in ints) {
  for(j in ints) {
    i2 <- trunc2int53(i)
    j2 <- trunc2int53(j)
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
    i2 <- trunc2int53(i)
    j2 <- trunc2int53(j)
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
      i2 <- trunc2int53(abs(i))
    }
    if(isTRUE(j > int53max || j < int53min)) {
      j2 <- NA_real_
    }
    else {
      j2 <- trunc2int53(abs(j))
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


# pmin/pmax ====

for(i in ints) {
  for(j in ints) {
    i2 <- trunc2int53(i)
    j2 <- trunc2int53(j)
    expect_equal(
      bc.i(i, j, "pmin"),
      pmin(i2, j2)
    ) |> errorfun()
    expect_equal(
      bc.i(i, j, "pmax"),
      pmax(i2, j2)
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
  }
}
