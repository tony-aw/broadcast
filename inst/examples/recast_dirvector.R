

# basic usage ====
x <- setNames(1:10, letters[1:10])
print(x)

recast_dirvector(x, 2L, broadcaster = TRUE)
recast_dirvector(x, 3L, broadcaster = TRUE)
recast_dirvector(x, 1L, 2L, TRUE, letters[11:20])


# modify an array along one specific dimension ====
x <- array(sample(0:9), c(3,3,3))
print(x)

# add 10 to second column, and 100 to third column:
v <- recast_dirvector(c(0, 10, 100), 2, 2, TRUE)
x + v

# add 10 to second row, and 100 to third row:
v <- recast_dirvector(v, 1L)
x + v


# add 10 to second layer, and 100 to third layer:
v <- recast_dirvector(v, 3L)
x + v

