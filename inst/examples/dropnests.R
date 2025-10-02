
x <- list(
  a = list(list(list(list(1:10)))),
  b = list(1:10)
)

print(x)

dropnests(x)


# recurse_all demonstration ====
x <- list(
  a = list(list(list(list(1:10)))),
  b = data.frame(month.abb, month.name),
  c = data.frame(month.abb)
)

dropnests(x) # by default, recurse_all = FALSE

dropnests(x, recurse_all = TRUE)


# maxdepth demonstration ====
x <- list(
  a = list(list(list(list(1:10)))),
  b = list(1:10)
)
print(x)

dropnests(x) # by default, maxdepth = 16

dropnests(x, maxdepth = 3L)

dropnests(x, maxdepth = 1L) # returns `x` unchanged

