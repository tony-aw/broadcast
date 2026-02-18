
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
x.data <- list(
  sample(c(TRUE, FALSE), 100, TRUE),
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  rep(NA, 100),
  sample(-10:10, 100, TRUE),
  sample(c(-10:10, NA), 100, TRUE),
  rep(NA_integer_, 100),
  sample(c(rnorm(98), Inf, -Inf)),
  sample(c(rnorm(96), NA, NaN, -Inf, Inf)),
  rep(NA_real_, 100),
  sample(c(rnorm(98), Inf, -Inf)) + sample(c(rnorm(98), Inf, -Inf)) * -1i,
  sample(c(rnorm(96), NA, NaN, -Inf, Inf)) + sample(c(rnorm(96), NA, NaN, -Inf, Inf)) * -1i,
  rep(NA_complex_, 100),
  sample(month.name, 100, TRUE),
  sample(c(month.name, NA), 100, TRUE),
  rep(NA_character_, 100)
)
x.names <- sample(letters, 100, TRUE)
x.dimnames <- list(sample(letters, 10), sample(LETTERS, 10))


# vectors ====
for(i in seq_along(x.data)) {
  for(j in c(TRUE, FALSE)) {
    x <- x.data[[i]]
    if(j) {
      names(x) <- x.names
    }
    
    # any:
    expect_equal(
      anyNA(x),
      checkNA(x, "any")
    ) |> errorfun()
    expect_equal(
      any(!is.na(x)),
      checkNA(x, "any", TRUE)
    ) |> errorfun()
    
    # all:
    expect_equal(
      all(is.na(x)),
      checkNA(x, "all")
    ) |> errorfun()
    expect_equal(
      all(!is.na(x)),
      checkNA(x, "all", TRUE)
    ) |> errorfun()
    
    # count:
    expect_equal(
      sum(is.na(x)),
      checkNA(x, "count")
    ) |> errorfun()
    expect_equal(
      sum(!is.na(x)),
      checkNA(x, "count", inv = TRUE)
    ) |> errorfun()
    
    # logical:
    expect_equal(
      is.na(x),
      checkNA(x, "logical")
    ) |> errorfun()
    expect_equal(
      !is.na(x),
      checkNA(x, "logical", inv = TRUE)
    ) |> errorfun()
    
    # raw:
    expect_equal(
      as_raw(is.na(x)),
      checkNA(x, "raw")
    ) |> errorfun()
    expect_equal(
      as_raw(!is.na(x)),
      checkNA(x, "raw", inv = TRUE)
    ) |> errorfun()
    
    # which:
    expect_equal(
      which(is.na(x)),
      checkNA(x, "which")
    ) |> errorfun()
    expect_equal(
      which(!is.na(x)),
      checkNA(x, "which", inv = TRUE)
    ) |> errorfun()
    
    
    # first:
    expected <- which(is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNA(x, "first")
    ) |> errorfun()
    expected <- which(!is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNA(x, "first", inv = TRUE)
    ) |> errorfun()
    
    # last:
    expected <- which(is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNA(x, "last")
    ) |> errorfun()
    expected <- which(!is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNA(x, "last", inv = TRUE)
    ) |> errorfun()
    
    enumerate <- enumerate + 16L
  }
}



# arrays ====
for(i in seq_along(x.data)) {
  for(j in c(TRUE, FALSE)) {
    x <- array(x.data[[i]], c(10, 10))
    
    if(j) {
      dimnames(x) <- x.dimnames
    }
    
    # any:
    expect_equal(
      anyNA(x),
      checkNA(x, "any")
    ) |> errorfun()
    expect_equal(
      any(!is.na(x)),
      checkNA(x, "any", TRUE)
    ) |> errorfun()
    
    # all:
    expect_equal(
      all(is.na(x)),
      checkNA(x, "all")
    ) |> errorfun()
    expect_equal(
      all(!is.na(x)),
      checkNA(x, "all", TRUE)
    ) |> errorfun()
    
    # count:
    expect_equal(
      sum(is.na(x)),
      checkNA(x, "count")
    ) |> errorfun()
    expect_equal(
      sum(!is.na(x)),
      checkNA(x, "count", inv = TRUE)
    ) |> errorfun()
    
    # logical:
    expect_equal(
      is.na(x),
      checkNA(x, "logical")
    ) |> errorfun()
    expect_equal(
      !is.na(x),
      checkNA(x, "logical", inv = TRUE)
    ) |> errorfun()
    
    # raw:
    expect_equal(
      as_raw(is.na(x)),
      checkNA(x, "raw")
    ) |> errorfun()
    expect_equal(
      as_raw(!is.na(x)),
      checkNA(x, "raw", inv = TRUE)
    ) |> errorfun()
    
    # which:
    expect_equal(
      which(is.na(x)),
      checkNA(x, "which")
    ) |> errorfun()
    expect_equal(
      which(!is.na(x)),
      checkNA(x, "which", inv = TRUE)
    ) |> errorfun()
    
    # first:
    expected <- which(is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNA(x, "first")
    ) |> errorfun()
    expected <- which(!is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNA(x, "first", inv = TRUE)
    ) |> errorfun()
    
    # last:
    expected <- which(is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNA(x, "last")
    ) |> errorfun()
    expected <- which(!is.na(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNA(x, "last", inv = TRUE)
    ) |> errorfun()
    
    enumerate <- enumerate + 16L
    
  }
}


# which 64bit (for long vectors) ====
whichNA_64 <- broadcast:::.rcpp_checkmissing_which64

for(i in seq_along(x.data)) {
  
  x <- x.data[[i]]
  
  size <- checkNA(x, "count")
  if(size > 0) {
    expect_equal(
      which(is.na(x)),
      whichNA_64(x, FALSE, size)
    ) |> errorfun()
  }
  
  size <- checkNA(x, "count", TRUE)
  if(size > 0) {
    expect_equal(
      which(!is.na(x)),
      whichNA_64(x, TRUE, size)
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 2L
  
}


# zero-len ====
x.data <- list(
  logical(0L),
  integer(0L),
  double(0L),
  complex(0L),
  character(0L)
)

for(i in seq_along(x.data)) {
  for(j in c(TRUE, FALSE)) {
    x <- x.data[[i]]
    if(j) {
      dim(x) <- c(0L, 0L)
    }
    
    # any:
    expect_equal(
      anyNA(x),
      checkNA(x, "any")
    ) |> errorfun()
    expect_equal(
      any(!is.na(x)),
      checkNA(x, "any", TRUE)
    ) |> errorfun()
    
    # all:
    expect_equal(
      all(is.na(x)),
      checkNA(x, "all")
    ) |> errorfun()
    expect_equal(
      all(!is.na(x)),
      checkNA(x, "all", TRUE)
    ) |> errorfun()
    
    # count:
    expect_equal(
      sum(is.na(x)),
      checkNA(x, "count")
    ) |> errorfun()
    expect_equal(
      sum(!is.na(x)),
      checkNA(x, "count", inv = TRUE)
    ) |> errorfun()
    
    # logical:
    expect_equal(
      is.na(x),
      checkNA(x, "logical")
    ) |> errorfun()
    expect_equal(
      !is.na(x),
      checkNA(x, "logical", inv = TRUE)
    ) |> errorfun()
    
    # raw:
    expect_equal(
      as_raw(is.na(x)),
      checkNA(x, "raw")
    ) |> errorfun()
    expect_equal(
      as_raw(!is.na(x)),
      checkNA(x, "raw", inv = TRUE)
    ) |> errorfun()
    
    # which:
    expect_equal(
      which(is.na(x)),
      checkNA(x, "which")
    ) |> errorfun()
    expect_equal(
      which(!is.na(x)),
      checkNA(x, "which", inv = TRUE)
    ) |> errorfun()
    
    
    # first:
    expect_equal(
      0L,
      checkNA(x, "first")
    ) |> errorfun()
    expect_equal(
      0L,
      checkNA(x, "first", inv = TRUE)
    ) |> errorfun()
    
    # last:
    expect_equal(
      0L,
      checkNA(x, "last")
    ) |> errorfun()
    expect_equal(
      0L,
      checkNA(x, "last", inv = TRUE)
    ) |> errorfun()
    
    enumerate <- enumerate + 16L
    
  }
}


# broadcaster attribute ====
x <- sample(c(1:10, NA))
expect_false(
  checkNA(x, "logical") |> broadcaster()
)
broadcaster(x) <- TRUE
expect_true(
  checkNA(x, "logical") |> broadcaster()
)
enumerate <- enumerate + 2L


# errors ====
expect_error(
  checkNA(as.raw(0:10), "raw"),
  pattern = "unsupported type for `x` given"
)
expect_error(
  checkNA(as.list(0:10), "raw"),
  pattern = "unsupported type for `x` given"
)
expect_error(
  checkNA(~ 0:10, "raw"),
  pattern = "unsupported type for `x` given"
)
enumerate <- enumerate + 3L

expect_error(
  checkNA(0:10, NA_character_),
  pattern = "`op` must be a single non-missing string"
)
expect_error(
  checkNA(0:10, letters),
  pattern = "`op` must be a single non-missing string"
)
expect_error(
  checkNA(0:10, 1L),
  pattern = "`op` must be a single non-missing string"
)
expect_error(
  checkNA(0:10, "foo"),
  pattern = "unsupported operator given"
)
enumerate <- enumerate + 4L

expect_error(
  checkNA(0:10, "raw", inv = NA),
  pattern = "`inv` must be `TRUE` or `FALSE`"
)
expect_error(
  checkNA(0:10, "raw", inv = c(TRUE, FALSE)),
  pattern = "`inv` must be `TRUE` or `FALSE`"
)
enumerate <- enumerate + 2L


