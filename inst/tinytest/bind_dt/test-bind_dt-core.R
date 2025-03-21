
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dimnames <- function(n) {
  lapply(n, \(i)sample(letters, i))
}


# cbind/rbind ====
x <- data.table::data.table(
  a = 1:10,
  b = letters[1:10]
)
y <- data.table::data.table(
  a = 11:20,
  b = LETTERS[1:10]
)
expect_equal(
  bind_dt(list(x, y), 1L),
  data.table::rbindlist(list(x, y))
)
expect_equal(
  bind_dt(list(x, y), 2L),
  data.table::data.table(x, y, check.names = TRUE)
)
enumerate <- enumerate + 2L


