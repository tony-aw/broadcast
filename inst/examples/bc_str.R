x.dim <- c(10:8)
x.len <- prod(x.dim)
x <- array(letters, x.dim)
y <- array(letters, c(10,1,1))

bc.str(x, y, "==")
bc.str(x, y, "!=")

bc.str(x, y, "+")

bc.str(array(letters), array(letters), "==")
bc.str(array(letters), array(letters), "!=")
