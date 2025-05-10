
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

.is_array_like <- broadcast:::.is_array_like
.is_supported_type <- broadcast:::.is_supported_type

x <- array(1:10)
expect_equal(
  .is_array_like(x),
  TRUE
)
expect_equal(
  .is_array_like(as.vector(x)),
  TRUE
)
expect_equal(
  .is_supported_type(as.factor(x)),
  FALSE
)
enumerate <- enumerate + 3L
