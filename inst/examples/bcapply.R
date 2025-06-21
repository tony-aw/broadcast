
x.dim <- c(5, 3, 2)
x.len <- prod(x.dim)

gen <- function(n) sample(list(letters, month.abb, 1:10), n, TRUE)

x <- array(gen(10), x.dim)
y <- array(gen(10), c(5, 1, 1))

f <- function(x, y) paste(x, y)
bcapply(x, y, f, v = "character")
