# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# lhs ====
x <- 1:(2^31)
expect_error(
  x %orientbc<-% 3L,
  pattern = "unsupported length of `lhs`"
)
x <- integer(0L)
expect_error(
  x %orientbc<-% 3L,
  pattern = "unsupported length of `lhs`"
)
x <- 1L
expect_error(
  x %orientbc<-% 3L,
  pattern = "unsupported length of `lhs`"
)

x <- data.frame(a = 1:10, b = 11:20)
expect_error(
  x %orientbc<-% 3L,
  pattern = "`lhs` must be a vector or array"
)
x <- array(1:27, c(3,3,3))
expect_error(
  x %orientbc<-% 3L,
  pattern = "`lhs` is a multi-dimensional array"
)

enumerate <- enumerate + 5L

# rhs ====
x <- as.array(1:10)
pattern <- "`rhs` must be a integer vector of length 1 or 2 without missing values"
expect_error(
  x %orientbc<-% letters,
  pattern = pattern
)
expect_error(
  x %orientbc<-% 1:3,
  pattern = pattern
)
expect_error(
  x %orientbc<-% c(1, NA),
  pattern = pattern
)
enumerate <- enumerate + 4L

expect_error(
  x %orientbc<-% c(1.5, 2.5),
  pattern = "`rhs` must consist of only whole numbers"
)
enumerate <- enumerate + 1L


for(i in c(0, 17)) {
  for(j in c(0, 17)) {
    expect_error(
      x %orientbc<-% c(i, j),
      pattern = "`rhs` may not be larger than 16 or smaller than 1"
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}

for(i in 1:16) {
  for(j in i:16) {
    expect_silent(
      x %orientbc<-% c(i, j)
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}

for(i in 1:15) {
  for(j in (i+1):16) {
    expect_error(
      x %orientbc<-% c(j, i),
      pattern = "the orientation (`rhs[1]`) cannot be larger than the number of dimensions (`rhs[2]`)",
      fixed = TRUE
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}



# target of assignment expands to non-language object ====
expect_error(
  (1:10) %orientbc<-% c(1,2)
)
enumerate <- enumerate + 1L
