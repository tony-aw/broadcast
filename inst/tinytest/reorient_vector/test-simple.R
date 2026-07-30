# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


x <- setNames(1:10, letters[1:10])
x2 <- x
x %orientbc<-% 1L
expect_true(
  broadcaster(x)
)
expect_true(
  is.array(x)
)
expect_equal(
  dim(x), length(x)
)
expect_equal(
  dimnames(x)[[1L]], names(x2)
)
expect_equal(
  as.vector(x), as.vector(x2)
)

x <- setNames(1:10, letters[1:10])
x2 <- x
x %orientbc<-% 2L
expect_true(
  broadcaster(x)
)
expect_true(
  is.array(x)
)
expect_equal(
  dim(x), c(1L, length(x))
)
expect_equal(
  dimnames(x)[[2L]], names(x2)
)
expect_equal(
  as.vector(x), as.vector(x2)
)


x <- setNames(1:10, letters[1:10])
x2 <- x
x %orientbc<-% 3L
expect_true(
  broadcaster(x)
)
expect_true(
  is.array(x)
)
expect_equal(
  dim(x), c(1L, 1L, length(x))
)
expect_equal(
  dimnames(x)[[3L]], names(x2)
)
expect_equal(
  as.vector(x), as.vector(x2)
)

enumerate <- enumerate + 5L*3L
