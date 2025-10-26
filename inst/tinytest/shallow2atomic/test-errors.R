
enumerate <- 0L
x <- list(1:10, 1:9)


# bad ellipsis ====
expect_error(
  cast_shallow2atomic(x, foo = 1),
  pattern = "unkown arguments given"
)
enumerate <- enumerate + 4L


# bad input list ====
expect_error(
  cast_shallow2atomic(pairlist(1:10)),
  pattern = "`x` must be a shallow list"
)
expect_error(
  cast_shallow2atomic(list()),
  pattern = "cannot cast zero-length list"
)
expect_error(
  cast_shallow2atomic(list(1:10, list(1:10))),
  pattern = "`x` must be a shallow list"
)
expect_error(
  cast_shallow2atomic(
    list(NULL, raw(0), logical(0), integer(0), double(0), complex(0), character(0))
  ),
  pattern = "all elements of `x` have length zero"
)
expect_error(
  cast_shallow2atomic(list(1:2e15)),
  pattern = "long vectors not supported"
)
enumerate <- enumerate + 5L


# bad arrangement argument ====
expect_error(
  cast_shallow2atomic(x, list()),
  pattern = "`arrangement` must be 0, 1 or -1"
)
expect_error(
  cast_shallow2atomic(x, -1:1),
  pattern = "`arrangement` must be 0, 1 or -1"
)
expect_error(
  cast_shallow2atomic(x, NA_integer_),
  pattern = "`arrangement` must be 0, 1 or -1"
)
expect_error(
  cast_shallow2atomic(x, 3),
  pattern = "`arrangement` must be 0, 1 or -1"
)
enumerate <- enumerate + 4L


# bad padding argument ====
expect_error(
  cast_shallow2atomic(x, padding = list(NULL)),
  pattern = "`padding` must be an atomic scalar"
)
expect_error(
  cast_shallow2atomic(x, padding = 1:10),
  pattern = "`padding` must be an atomic scalar"
)
enumerate <- enumerate + 2L


# bad comnames_from argument ====
expect_error(
  cast_shallow2atomic(x, comnames_from = NA_integer_),
  pattern = "`comnames_from` must be either `NULL` or a scalar integer >= 1 and <= length(x)",
  fixed = TRUE
)
expect_error(
  cast_shallow2atomic(x, comnames_from = 0L),
  pattern = "`comnames_from` must be either `NULL` or a scalar integer >= 1 and <= length(x)",
  fixed = TRUE
)
expect_error(
  cast_shallow2atomic(x, comnames_from = 1:2),
  pattern = "`comnames_from` must be either `NULL` or a scalar integer >= 1 and <= length(x)",
  fixed = TRUE
)
expect_error(
  cast_shallow2atomic(x, comnames_from = 100),
  pattern = "`comnames_from` must be either `NULL` or a scalar integer >= 1 and <= length(x)",
  fixed = TRUE
)
enumerate <- enumerate + 4L


# strange types warning ====
foo <- "foo"
x <- list(~ foo, as.name(foo), as.symbol(foo))
expect_warning(
  cast_shallow2atomic(x),
  pattern = "expression-like elements found; coercing to character"
)
enumerate <- enumerate + 1L


