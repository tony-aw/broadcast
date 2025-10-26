
x <- array(sample(month.abb), c(10, 2))
y <- array(sample(1:10), c(10, 2, 3))

print(x)
print(y)

bc_strrep(x, y)
