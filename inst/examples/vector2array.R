
x <- setNames(1:27, c(letters, NA))
print(x)
y <- vector2array(x, 1L, 3L)
print(y)

undim(y)
