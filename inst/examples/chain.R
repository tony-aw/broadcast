
x <- 1:10
y <- 1:10
bc(~ x + y / x)
x + 1 # mathematically equivalent to the above, since x == y
bc(~ (x + y) / x)
2 * x/x # mathematically equivalent to the above, since x == y

dim(x) <- c(10, 1)
dim(y) <- c(1, 10)

bc(~ x + y / x)
bc(~ (x + y) / x)
bc(~ ifelse(x > 1, y, x)) # ifelse and apply are not overloaded
