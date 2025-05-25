x.dim <- c(4:2)
x.len <- prod(x.dim)
x.data <- as.raw(0:10)
y.data <- as.raw(10:0)
x <- array(x.data, x.dim)
y <- array(y.data, c(4,1,1))

bc.raw(x, y, "==")
bc.raw(x, y, "!=")
bc.raw(x, y, "<")
bc.raw(x, y, ">")
bc.raw(x, y, "<=")
bc.raw(x, y, ">=")

