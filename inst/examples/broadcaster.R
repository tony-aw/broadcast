
# maths ====

x <- sample(as.double(1:10))
y <- sample(as.double(1:10))
dim(x) <- c(10, 1)
dim(y) <- c(1, 10)
mbroadcasters(c("x", "y"), TRUE)



x + y / x
(x + y) / x

(x + y) * x


# relational operators ====
x <- as.double(1:10)
y <- array(as.double(1:10), c(1, 10))
mbroadcasters(c("x", "y"), TRUE)

x == y
x != y
x < y
x > y
x <= y
x >= y


