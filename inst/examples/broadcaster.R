
# maths ====

x <- 1:10
y <- 1:10
dim(x) <- c(10, 1)
dim(y) <- c(1, 10)
broadcaster(x) <- TRUE
broadcaster(y) <- TRUE



x + y / x
(x + y) / x

(x + y) * x


# relational operators ====
x <- 1:10
y <- array(1:10, c(1, 10))
broadcaster(x) <- TRUE
broadcaster(y) <- TRUE

x == y
x != y
x < y
x > y
x <= y
x >= y

