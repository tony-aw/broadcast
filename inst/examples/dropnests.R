
x <- list(
  a = list(list(list(list(1:10)))),
  b = list(1:10)
)
print(x)


dropnests(x)


# recurse_classed demonstration ====
x <- list(
  a = list(list(list(list(1:10)))),
  b = data.frame(month.abb, month.name),
  c = data.frame(month.abb)
)


dropnests(x) # by default, recurse_classed = FALSE

dropnests(x, recurse_classed = TRUE)


# maxdepth demonstration ====
x <- list(
  a = list(list(list(list(1:10)))),
  b = list(1:10)
)
print(x)


dropnests(x) # by default, maxdepth = 32

dropnests(x, maxdepth = 3L)

dropnests(x, maxdepth = 1L) # returns `x` unchanged

