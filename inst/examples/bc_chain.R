
# maths ====

x <- 1:10
y <- 1:10
bc_chain(~ x + y / x)
x + 1 # mathematically equivalent to the above, since x == y
bc_chain(~ (x + y) / x)
2 * x/x # mathematically equivalent to the above, since x == y

dim(x) <- c(10, 1)
dim(y) <- c(1, 10)

bc_chain(~ x + y / x)
bc_chain(~ (x + y) / x)
bc_chain(~ ifelse(x > 1, y, x)) # ifelse and apply are not overloaded



# relational operators ====
x <- 1:10
y <- array(1:10, c(1, 10))

bc_chain(~ x == y)
bc_chain(~ x != y)
bc_chain(~ x < y)
bc_chain(~ x > y)
bc_chain(~ x <= y)
bc_chain(~ x >= y)

