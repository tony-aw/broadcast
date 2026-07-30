# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.as.broadcaster <- broadcast:::.as.broadcaster

# along rows ====
x <- matrix(1, 5, 4)
v <- 1:5
v %orientbc<-% 1L

expect_equivalent(
  x * v,
  .as.broadcaster(cbind(1:5, 1:5, 1:5, 1:5))
)
enumerate <- enumerate + 1L


# along columns ====
x <- matrix(1, 5, 4)
v <- 1:4
v %orientbc<-% 2L

expect_equivalent(
  x * v,
  .as.broadcaster(rbind(1:4, 1:4, 1:4, 1:4, 1:4))
)
enumerate <- enumerate + 1L

