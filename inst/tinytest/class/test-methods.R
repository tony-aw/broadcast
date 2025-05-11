
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# sub-setting ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
expect_equal(
  broadcast:::.as.broadcaster(x[1:2, ,drop = FALSE]),
  y[1:2, , drop = FALSE]
 
)
expect_equal(
  broadcast:::.as.broadcaster(x[,1:2 ,drop = FALSE]),
  y[,1:2 , drop = FALSE]
  
)
expect_equal(
  broadcast:::.as.broadcaster(x[2:3]),
  y[2:3]
)

enumerate <- enumerate + 3L



#  replacement ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
x[1] <- -1
y[1] <- -1
expect_equal(
  broadcast:::.as.broadcaster(x),
  y
)
enumerate <- enumerate + 3L

