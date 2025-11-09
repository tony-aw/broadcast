
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- array(1:2)
grp <- factor(c("a", "b"))
expect_equal(
  acast(x, 1, grp),
  array(c(1, 2), c(1, 2), list(NULL, c(levels(grp))))
)
enumerate <- enumerate + 1L
