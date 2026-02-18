
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x.data <- list(
  rep(list(NULL), 100),
  sample(list(letters, LETTERS, month.abb, month.name, NULL), 100, TRUE),
  sample(list(letters, LETTERS, month.abb, month.name), 100, TRUE)
)
x.dimnames <- list(sample(letters, 10), sample(LETTERS, 10))
x.names <- sample(letters, 100, TRUE)

basefun <- function(x) {
  out <- sapply(x, is.null)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


# vectors ====

for(i in seq_along(x.data)) {
  for(j in c(TRUE, FALSE)) {
    x <- x.data[[i]]
    if(j) {
      names(x) <- x.names
    }
    
    # any:
    expect_equal(
      any(basefun(x)),
      checkNULL(x, "any")
    ) |> errorfun()
    expect_equal(
      any(!basefun(x)),
      checkNULL(x, "any", TRUE)
    ) |> errorfun()
    
    # all:
    expect_equal(
      all(basefun(x)),
      checkNULL(x, "all")
    ) |> errorfun()
    expect_equal(
      all(!basefun(x)),
      checkNULL(x, "all", TRUE)
    ) |> errorfun()
    
    # count:
    expect_equal(
      sum(basefun(x)),
      checkNULL(x, "count")
    ) |> errorfun()
    expect_equal(
      sum(!basefun(x)),
      checkNULL(x, "count", inv = TRUE)
    ) |> errorfun()
    
    # logical:
    expect_equal(
      basefun(x),
      checkNULL(x, "logical")
    ) |> errorfun()
    expect_equal(
      !basefun(x),
      checkNULL(x, "logical", inv = TRUE)
    ) |> errorfun()
    
    # raw:
    expect_equal(
      as_raw(basefun(x)),
      checkNULL(x, "raw")
    ) |> errorfun()
    expect_equal(
      as_raw(!basefun(x)),
      checkNULL(x, "raw", inv = TRUE)
    ) |> errorfun()
    
    # which:
    expect_equal(
      which(basefun(x)),
      checkNULL(x, "which")
    ) |> errorfun()
    expect_equal(
      which(!basefun(x)),
      checkNULL(x, "which", inv = TRUE)
    ) |> errorfun()
    
    
    # first:
    expected <- which(basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNULL(x, "first")
    ) |> errorfun()
    expected <- which(!basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNULL(x, "first", inv = TRUE)
    ) |> errorfun()
    
    # last:
    expected <- which(basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNULL(x, "last")
    ) |> errorfun()
    expected <- which(!basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNULL(x, "last", inv = TRUE)
    ) |> errorfun()
    
    enumerate <- enumerate + 16L
    
  }
}



# arrays ====
for(i in seq_along(x.data)) {
  for(j in c(TRUE, FALSE)) {
    x <- array(x.data[[i]], dim=c(10,10))
    if(j) {
      dimnames(x) <- x.dimnames
    }
    
    
    # any:
    expect_equal(
      any(basefun(x)),
      checkNULL(x, "any")
    ) |> errorfun()
    expect_equal(
      any(!basefun(x)),
      checkNULL(x, "any", TRUE)
    ) |> errorfun()
    
    # all:
    expect_equal(
      all(basefun(x)),
      checkNULL(x, "all")
    ) |> errorfun()
    expect_equal(
      all(!basefun(x)),
      checkNULL(x, "all", TRUE)
    ) |> errorfun()
    
    # count:
    expect_equal(
      sum(basefun(x)),
      checkNULL(x, "count")
    ) |> errorfun()
    expect_equal(
      sum(!basefun(x)),
      checkNULL(x, "count", inv = TRUE)
    ) |> errorfun()
    
    # logical:
    expect_equal(
      basefun(x),
      checkNULL(x, "logical")
    ) |> errorfun()
    expect_equal(
      !basefun(x),
      checkNULL(x, "logical", inv = TRUE)
    ) |> errorfun()
    
    # raw:
    expect_equal(
      as_raw(basefun(x)),
      checkNULL(x, "raw")
    ) |> errorfun()
    expect_equal(
      as_raw(!basefun(x)),
      checkNULL(x, "raw", inv = TRUE)
    ) |> errorfun()
    
    # which:
    expect_equal(
      which(basefun(x)),
      checkNULL(x, "which")
    ) |> errorfun()
    expect_equal(
      which(!basefun(x)),
      checkNULL(x, "which", inv = TRUE)
    ) |> errorfun()
    
    
    # first:
    expected <- which(basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNULL(x, "first")
    ) |> errorfun()
    expected <- which(!basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[1L]
    expect_equal(
      expected,
      checkNULL(x, "first", inv = TRUE)
    ) |> errorfun()
    
    # last:
    expected <- which(basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNULL(x, "last")
    ) |> errorfun()
    expected <- which(!basefun(x)) |> as.vector()
    if(length(expected) == 0L) expected <- 0L
    expected <- expected[length(expected)]
    expect_equal(
      expected,
      checkNULL(x, "last", inv = TRUE)
    ) |> errorfun()
    
    enumerate <- enumerate + 16L
    
  }
}



# zero-len ====
for(j in c(TRUE, FALSE)) {
  x <- vector("list", 0L)
  if(j) {
    dim(x) <- c(0L, 0L)
  }
  
  
  # any:
  expect_equal(
    any(basefun(x)),
    checkNULL(x, "any")
  ) |> errorfun()
  expect_equal(
    any(!basefun(x)),
    checkNULL(x, "any", TRUE)
  ) |> errorfun()
  
  # all:
  expect_equal(
    all(basefun(x)),
    checkNULL(x, "all")
  ) |> errorfun()
  expect_equal(
    all(!basefun(x)),
    checkNULL(x, "all", TRUE)
  ) |> errorfun()
  
  # count:
  expect_equal(
    0L,
    checkNULL(x, "count")
  ) |> errorfun()
  expect_equal(
    0L,
    checkNULL(x, "count", inv = TRUE)
  ) |> errorfun()
  
  # logical:
  expected <- logical(0L)
  if(j) {
    dim(expected) <- c(0L, 0L)
  }
  expect_equal(
    expected,
    checkNULL(x, "logical")
  ) |> errorfun()
  expect_equal(
    expected,
    checkNULL(x, "logical", inv = TRUE)
  ) |> errorfun()
  
  # raw:
  expected <- raw(0L)
  if(j) {
    dim(expected) <- c(0L, 0L)
  }
  expect_equal(
    expected,
    checkNULL(x, "raw")
  ) |> errorfun()
  expect_equal(
    expected,
    checkNULL(x, "raw", inv = TRUE)
  ) |> errorfun()
  
  # which:
  expect_equal(
    which(as.logical(basefun(x))),
    checkNULL(x, "which")
  ) |> errorfun()
  expect_equal(
    which(!as.logical(basefun(x))),
    checkNULL(x, "which", inv = TRUE)
  ) |> errorfun()
  
  # first:
  expect_equal(
    0L,
    checkNULL(x, "first")
  ) |> errorfun()
  expect_equal(
    0L,
    checkNULL(x, "first", inv = TRUE)
  ) |> errorfun()
  
  # last:
  expect_equal(
    0L,
    checkNULL(x, "last")
  ) |> errorfun()
  expect_equal(
    0L,
    checkNULL(x, "last", inv = TRUE)
  ) |> errorfun()
  
  enumerate <- enumerate + 16L
  
}


# broadcaster attribute ====
x <- sample(as.list(c(1:10, NA)))
expect_false(
  checkNULL(x, "logical") |> broadcaster()
)
broadcaster(x) <- TRUE
expect_true(
  checkNULL(x, "logical") |> broadcaster()
)
enumerate <- enumerate + 2L


# errors ====
expect_error(
  checkNULL(0:10, "raw"),
  pattern = "unsupported type for `x` given"
)
expect_error(
  checkNULL(~ 0:10, "raw"),
  pattern = "unsupported type for `x` given"
)
enumerate <- enumerate + 2L

x <- as.list(0:10)
expect_error(
  checkNULL(x, NA_character_),
  pattern = "`op` must be a single non-missing string"
)
expect_error(
  checkNULL(x, letters),
  pattern = "`op` must be a single non-missing string"
)
expect_error(
  checkNULL(x, 1L),
  pattern = "`op` must be a single non-missing string"
)
expect_error(
  checkNULL(x, "foo"),
  pattern = "unsupported operator given"
)
enumerate <- enumerate + 4L

expect_error(
  checkNULL(x, "raw", inv = NA),
  pattern = "`inv` must be `TRUE` or `FALSE`"
)
expect_error(
  checkNULL(x, "raw", inv = c(TRUE, FALSE)),
  pattern = "`inv` must be `TRUE` or `FALSE`"
)
enumerate <- enumerate + 2L

