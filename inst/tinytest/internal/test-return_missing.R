
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.return_missing <- broadcast:::.return_missing

# zero-length ====
for(i in c("logical", "integer", "double", "complex", "character", "list")) {
  x <- vector(i, 0L)
  expect_equal(
    .return_missing(x),
    vector(i, 0L)
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

# logical ====
y <- c(TRUE, FALSE, NA)
for(iSample in 1:10) {
  for(iLen in 1:10) {
    x <- sample(y, iLen, TRUE)
    expect_equal(
      .return_missing(x),
      rep(NA, length(x))
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# integer ====
y <- sample(c(1:10, NA))
for(iSample in 1:10) {
  for(iLen in 1:10) {
    x <- sample(y, iLen, TRUE)
    expect_equal(
      .return_missing(x),
      rep(NA_integer_, length(x))
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# double ====
y <- sample(c(1:10, NA, NaN, Inf, -Inf))
for(iSample in 1:10) {
  for(iLen in 1:10) {
    x <- sample(y, iLen, TRUE)
    expect_equal(
      .return_missing(x),
      rep(NA_real_, length(x))
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# complex ====
y <- sample(c(1:10, NA, NaN, Inf, -Inf))
y <- y + -1i * y
for(iSample in 1:10) {
  for(iLen in 1:10) {
    x <- sample(y, iLen, TRUE)
    expect_equal(
      .return_missing(x),
      rep(NA_complex_, length(x))
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# character ====
y <- sample(c(month.abb, NA))
for(iSample in 1:10) {
  for(iLen in 1:10) {
    x <- sample(y, iLen, TRUE)
    expect_equal(
      .return_missing(x),
      rep(NA_character_, length(x))
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# list ====
y <- as.list(sample(c(month.abb, NA)))
for(iSample in 1:10) {
  for(iLen in 1:10) {
    x <- sample(y, iLen, TRUE)
    expect_equal(
      .return_missing(x),
      rep(list(NULL), length(x))
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}

