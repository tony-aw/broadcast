

# basic usage ====
x <- setNames(1:10, letters[1:10])
print(x)

x %orientbc<-% 2L
print(x)
x %orientbc<-% 3L
print(x)
x %orientbc<-% 1:2
print(x)
x %orientbc<-% c(1, 3)
print(x)



# modify an array along one specific dimension ====
x <- array(sample(0:9), c(3,3,3))
print(x)

# add 10 to second column, and 100 to third column:
v <- vector2array(c(0, 10, 100), 2, 2, TRUE)
x + v

# add 10 to second row, and 100 to third row:
v %orientbc<-% 1L
x + v

# add 10 to second layer, and 100 to third layer:
v %orientbc<-% 3L
x + v

