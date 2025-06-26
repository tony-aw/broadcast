
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  
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



# sub-setting2 ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
expect_equal(
  broadcast:::.as.broadcaster(x[[1, 1]]),
  y[[1, 1]]
  
)
enumerate <- enumerate + 1L




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



#  replacement2 ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
x[[1]] <- -1
y[[1]] <- -1
expect_equal(
  broadcast:::.as.broadcaster(x),
  y
)
enumerate <- enumerate + 3L


# errors ====
x <- 1:10
expect_error(
  broadcast:::`[.broadcaster`(x, 1),
  pattern = "malformed broadcaster"
)
expect_error(
  broadcast:::`[[.broadcaster`(x, 1),
  pattern = "malformed broadcaster"
)
