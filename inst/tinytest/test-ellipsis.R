# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops

x <- 1:10

# main tests ====

expect_error(
  acast(x, 2L, foo = TRUE),
  pattern = "unkown arguments given"
)

expect_error(
  hier2dim(x, foo = TRUE),
  pattern = "unkown arguments given"
)

expect_error(
  hiernames2dimnames(x, foo = TRUE),
  pattern = "unkown arguments given"
)

expect_error(
  cast_hier2dim(x, foo = TRUE),
  pattern = "unkown arguments given"
)

expect_error(
  dropnests(x, foo = TRUE),
  pattern = "unkown arguments given"
)

expect_error(
  cast_dim2flat(x, foo = TRUE),
  pattern = "unkown arguments given"
)

expect_error(
  cast_dim2hier(x, foo = TRUE),
  pattern = "unkown arguments given"
)

enumerate <- enumerate + 7L


# recurse_classed tests ====

expect_error(
  hier2dim(x, recurse_classed = TRUE),
  pattern = "`recurse_classed` has been replaced with `recurse_all`"
)

expect_error(
  hiernames2dimnames(x, recurse_classed = TRUE),
  pattern = "`recurse_classed` has been replaced with `recurse_all`"
)

expect_error(
  cast_hier2dim(x, recurse_classed = TRUE),
  pattern = "`recurse_classed` has been replaced with `recurse_all`"
)

expect_error(
  dropnests(x, recurse_classed = TRUE),
  pattern = "`recurse_classed` has been replaced with `recurse_all`"
)

enumerate <- enumerate + 4L


