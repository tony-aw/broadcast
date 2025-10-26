
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- list(as.character(1:10), as.character(1:2))
expect_equal(
  cast_shallow2atomic(x, 1, padding = -1),
  cbind(as.character(1:10), as.character(c(1:2, rep(-1, 8))))
)

expect_equal(
  cast_shallow2atomic(x, -1, padding = -1),
  rbind(as.character(1:10), as.character(c(1:2, rep(-1, 8))))
)

enumerate <- enumerate + 2L
