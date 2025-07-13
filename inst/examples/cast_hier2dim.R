

# evenly nested list, in2out = TRUE ====
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
    ),
    class3 = list(
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
    ),
    class3 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  )
)

# predict what dimensions `x` would have if casted as dimensional:
hier2dim(x) # all have name "no padding", because `x` is an evenly nested list

x2 <- cast_hier2dim(x) # cast as dimensional

# since the original list uses the same names for all elements within the same depth,
# dimnames can be set easily:
# because in2out = TRUE, we go from the deeper names to the surface names
dimnames(x2) <- list(
  names( x[[1]][[1]] ),
  names( x[[1]] ),
  names( x )
)
print(x2) # very compact, maybe too compact...?

# print a small portion of the list, but less compact:
cast_dim2flat(x2[, 1:2, "group1", drop = FALSE])


# evenly nested list, in2out = FALSE ====
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
    ),
    class3 = list(
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
    ),
    class3 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  )
)

# predict what dimensions `x` would have if casted as dimensional:
hier2dim(x) # all have name "no padding", because `x` is an evenly nested list

x2 <- cast_hier2dim(x, in2out = FALSE) # cast as dimensional

# since the original list uses the same names for all elements within the same depth,
# dimnames can be set easily:
# because in2out = FALSE, we go from the shallow names to the deeper names
dimnames(x2) <- list(
  names( x ),
  names( x[[1]] ),
  names( x[[1]][[1]] )
)
print(x2) # very compact, maybe too compact...?

# print a small portion of the list, but less compact:
cast_dim2flat(x2["group1", 1:2, , drop = FALSE])



# unevenly nested list ====

# this list is UNEVENLY nested:
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
      sex = sample(c("M", "F", NA), 10, TRUE)
    ),
    class3 = list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80),
      sex = sample(c("M", "F", NA), 10, TRUE)
    )
  )
)
# here `x` is UNEVENLY nested because:
# -> not all groups (surface level, or depth = 1) have the same number of classes.
# -> not all classes (depth = 2) have the same number of variables:
#     x$group2$class2 is missing variable "weight".

hier2dim(x) # as indicated here, the first 2 dimensions (i.e. rows and columns) will have padding

# casting this to a dimensional list will resulting in padding with `NULL`:
x2 <- cast_hier2dim(x)
print(x2)
# The `NULL` values are added for padding.
# This is because all slices of the same dimension need to have the same number of elements.  
# For example, all rows need to have the same number of columns.

# one can also use custom padding:
x2 <- cast_hier2dim(x, padding = list(~ "this is padding"))
print(x2)

# because `x` is unevenly nested, there is no guarantee you can set dimnames properly:
if(requireNamespace("tinytest")) {
  tinytest::expect_error(
    dimnames(x2) <- list(
      names( x[[1]][[1]] ),
      names( x[[1]] ),
      names( x )
    ),
    pattern = "length of 'dimnames' [2] not equal to array extent", # error message
    fixed = TRUE
  )
}

# in this case I know what the dimnames should be, so I can try like so:
dimnames(x2) <- list(
  c("height", "weight", "sex"),
  c("class1", "class2", "class3"),
  c("group1", "group2")
)

print(x2)
cast_dim2flat(x2[1:2, , , drop = FALSE])

# we can also use in2out = FALSE:
x2 <- cast_hier2dim(x, in2out = FALSE, padding = list(~ "this is padding"))
dimnames(x2) <- list( # notice the order here is reversed, because in2out = FALSE
  c("group1", "group2"),
  c("class1", "class2", "class3"),
  c("sex", "weight", "height")
  
)
print(x2)
cast_dim2flat(x2[, , 1:2, drop = FALSE])
