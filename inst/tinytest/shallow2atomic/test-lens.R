
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


x <- list(
  1:11, 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
)
x2 <- lapply(x, \(x){
  length(x) <- 11
  x
})
expect_equal(
  cast_shallow2atomic(x, 1),
  do.call(cbind, x2)
)

expect_equal(
  cast_shallow2atomic(x, -1),
  do.call(rbind, x2)
)
expect_equal(
  cast_shallow2atomic(x, 0),
  unlist(x)
)

enumerate <- enumerate + 3L
