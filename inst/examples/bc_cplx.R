x.dim <- c(4:2)
x.len <- prod(x.dim)
gen <- function() sample(c(rnorm(20), NA, NaN, Inf, -Inf))
x <- array(gen() + gen() * -1i, x.dim)
y <- array(gen() + gen() * -1i, c(4,1,1))

bc.cplx(x, y, "==")
bc.cplx(x, y, "!=")

bc.cplx(x, y, "+")

bc.cplx(array(gen() + gen() * -1i), array(gen() + gen() * -1i), "==")
bc.cplx(array(gen() + gen() * -1i), array(gen() + gen() * -1i), "!=")

x <- array(gen() + gen() * -1i)
y <- array(gen() + gen() * -1i)
bcr(x) <- bcr(y) <- TRUE
out <- x * y
bind_array(list(x = x, y = y, `x*y` = x*y, out = out), 2L)
