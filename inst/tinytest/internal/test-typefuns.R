
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

.is_array_like <- broadcast:::.is_array_like

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
  .is_array_like(as.factor(x)),
  FALSE
)
enumerate <- enumerate + 3L
