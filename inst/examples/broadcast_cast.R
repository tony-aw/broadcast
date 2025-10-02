
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

# predict dimensions x would have if casted as dimensional:
hier2dim(x, in2out = FALSE)

# cast x to dimensional list:
x2 <- cast_hier2dim(x, in2out = FALSE)

# set dimnames of dimensionally casted list:
dimnames(x2) <- hiernames2dimnames(x, in2out = FALSE)

# print result:
print(x2) # very compact
cast_dim2flat(x2) |> print() # less compact

# cast dimensional list back to nested/hierarchical list:
x3 <- cast_dim2hier(x2, in2out = FALSE, distr.names = TRUE)
print(x3)
