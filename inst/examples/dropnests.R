
x <- list(
  list(list(list(list(1:10)))),
  list(1:10)
)
print(x)

depth_range(x)
dropnests(x)


# recurse_classed demonstration ====
x <- list(
  list(list(list(list(1:10)))),
  data.frame(month.abb, month.name),
  data.frame(month.abb)
)

depth_range(x)
dropnests(x) # by default, recurse_classed = FALSE

depth_range(x, recurse_classed = TRUE)
dropnests(x, recurse_classed = TRUE)


# maxdepth demonstration ====
x <- list(
  list(list(list(list(1:10)))),
  list(1:10)
)
print(x)

depth_range(x)
dropnests(x) # by default, maxdepth = 32

depth_range(x, maxdepth = 3L)
dropnests(x, maxdepth = 3L)

depth_range(x, maxdepth = 1L)
dropnests(x, maxdepth = 1L) # returns `x` unchanged

