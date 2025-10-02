
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- list(
  group1 = list(
    class1 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    ),
    class2 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  ),
  group2 = list(
    class1 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    ),
    class2 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  )
)

expect_error(
  hiernames2dimnames(c(x, data.frame(letters, LETTERS))),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  hiernames2dimnames(c(x, array(list(letters, LETTERS)))),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  hiernames2dimnames(rep(list(NULL), 10)),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  hiernames2dimnames(list()),
  pattern = "cannot cast zero-length list"
)
expect_error(
  hiernames2dimnames(1:10),
  pattern = "`x` must be a list"
)
expect_error(
  hiernames2dimnames(x, in2out = NA),
  pattern = "`in2out` must be `TRUE` or `FALSE`"
)
expect_error(
  hiernames2dimnames(x, recurse_all = NA),
  pattern = "`recurse_all` must be `TRUE` or `FALSE`"
)
expect_error(
  hiernames2dimnames(matrix(as.list(1:10))),
  pattern = "`x` already has dimensions"
)
expect_error(
  hiernames2dimnames(as.list(1:10), maxdepth = NA),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  hiernames2dimnames(as.list(1:10), maxdepth = NA_integer_),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  hiernames2dimnames(as.list(1:10), maxdepth = 1:10),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  hiernames2dimnames(x, direction = NA),
  pattern = "`direction` must be 1 or -1"
)
expect_error(
  hiernames2dimnames(x, direction = c(1, -1)),
  pattern = "`direction` must be 1 or -1"
)

enumerate <- enumerate + 13L

