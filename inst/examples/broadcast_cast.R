
# recurse_all demonstration ====
x <- list(
  a = list(list(list(list(1:10)))),
  b = data.frame(month.abb, month.name),
  c = data.frame(month.abb),
  d = array(list(1), c(1,1,1))
)

dropnests(x) # by default, recurse_all = FALSE

dropnests(x, recurse_all = TRUE) # recurse_all = TRUE



# in2out demonstration ====
x <- list(
  group1 = list(
    class1 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    ),
    class2 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  ),
  group2 = list(
    class1 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    ),
    class2 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  )
)

# in2out = TRUE (default):
x2 <- cast_hier2dim(x)
dimnames(x2) <- hiernames2dimnames(x)
print(x2)
cast_dim2flat(x2[1,1,,drop = FALSE])

# in2out = FALSE:
x2 <- cast_hier2dim(x, in2out = FALSE)
dimnames(x2) <- hiernames2dimnames(x, in2out = FALSE)
print(x2)
cast_dim2flat(x2[1,1,,drop = FALSE])

