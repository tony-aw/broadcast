
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

x <- array(1:64, c(4,4,4))
grp <- as.factor(rep_len(1:3, 4))


# margin errors ====
expect_error(
  acast(x, 1:10, grp, TRUE),
  pattern = "`margin` must be an integer scalar"
)
expect_error(
  acast(x, NA_integer_, grp, TRUE),
  pattern = "`margin` must be an integer scalar"
)
expect_error(
  acast(x, -1L, grp, TRUE),
  pattern = "`margin` out of bounds"
)
expect_error(
  acast(x, 0, grp, TRUE),
  pattern = "`margin` out of bounds"
)
expect_error(
  acast(x, 4L, grp, TRUE),
  pattern = "`margin` out of bounds",
  fixed = TRUE
)
enumerate <- enumerate + 5L



# x errors ====
expect_error(
  acast(array(numeric(0L)), 1L, grp, TRUE),
  pattern = "zero-length or singular `x` not supported",
  fixed = TRUE
)
expect_error(
  acast(array(numeric(1L)), 1L, grp, TRUE),
  pattern = "zero-length or singular `x` not supported",
  fixed = TRUE
)
expect_error(
  acast(array(as.raw(0L), rep(3L, 15)), 1L, as.factor(c(1:3)), TRUE),
  pattern = "acasting would result in an array > 16 dimensions",
  fixed = TRUE
)
expect_error(
  acast(array(NA, c(1:3)), 1L, as.factor(1L), TRUE),
  pattern = "`dim(x)[margin]` must be >= 2",
  fixed = TRUE
)
enumerate <- enumerate + 4L



# grp errors ====
expect_error(
  acast(x, 1L, 1:2, TRUE),
  pattern = "`grp` must be a factor",
  fixed = TRUE
)
expect_error(
  acast(x, 1L, as.factor(1:10), TRUE),
  pattern = "length(grp) != dim(x)[margin]",
  fixed = TRUE
)
expect_error(
  acast(x, 1L, as.factor(1:10), TRUE),
  pattern = "length(grp) != dim(x)[margin]",
  fixed = TRUE
)
expect_error(
  acast(x, 1L, as.factor(rep(1L, nrow(x))), TRUE),
  pattern = "`grp` must have at least 2 unique values",
  fixed = TRUE
)
expect_silent(
  acast(x, 1L, as.factor(rep_len(1:2, nrow(x))), TRUE)
)
expect_error(
  acast(x, 1L, as.factor(c(1:3, NA)), TRUE),
  pattern = "`grp` cannot have NAs",
  fixed = TRUE
)
grp2 <- factor(c(1:3, NA), levels = c(1:3, NA), exclude = NULL) 
expect_error(
  acast(x, 1L, grp2, TRUE),
  pattern = "`grp` cannot have NAs",
  fixed = TRUE
)
enumerate <- enumerate + 7L



# fill errors ====
x <- array(1:64, c(4,4,4))
grp <- as.factor(rep_len(1:3, 4))
expect_error(
  acast(x, 1L, grp, NA),
  pattern = "`fill` must be `TRUE` or `FALSE`"
)
expect_error(
  acast(x, 1L, grp, c(TRUE, FALSE)),
  pattern = "`fill` must be `TRUE` or `FALSE`"
)
expect_error(
  acast(x, 1L, grp, TRUE, 1:10),
  pattern = "`fill_val` must be a single scalar"
)
expect_error(
  acast(x, 1L, grp, TRUE, list(NULL)),
  pattern = "`is.atomic(fill_val)` must match `is.atomic(x)`",
  fixed = TRUE
)
enumerate <- enumerate + 4L



# property errors ====
x <- array(1:64, c(4,4,4))
grp <- as.factor(c(1, 1, 2, 3))
expect_error(
  acast(x, 1L, grp, FALSE),
  pattern = "when `fill = FALSE`, all groups must have equal frequency"
)
expect_error(
  acast(as_raw(x), 1L, grp, TRUE),
  pattern = "typeof `raw` does not support NAs, so all groups must have equal frequency"
)
enumerate <- enumerate + 1L

