
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.C_check_conf_dim <- broadcast:::.C_check_conf_dim
# note: use double-typed lengths here,
# as integer-typed length are tested indirectly by most other tests anyways


# scalar by scalar ====
expect_equal(
  .C_check_conf_dim(NULL, NULL, 1.0, 1.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(1L, 1L, 1.0, 1.0),
  TRUE
)
enumerate <- enumerate + 1L


# vector by scalar ====
expect_equal(
  .C_check_conf_dim(NULL, NULL, 10.0, 1.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(10L, 1L, 10.0, 1.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(NULL, NULL, 1.0, 10.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(1L, 10L, 1.0, 10.0),
  TRUE
)
enumerate <- enumerate + 1L


# array by scalar ====
for(i in 1:10) {
  dims <- sample(1:10, i)
  expect_equal(
    .C_check_conf_dim(dims, NULL, prod(dims), 1.0),
    TRUE
  ) |> errorfun()
  expect_equal(
    .C_check_conf_dim(dims, rep(1L, i), prod(dims), 1.0),
    TRUE
  ) |> errorfun()
  
  expect_equal(
    .C_check_conf_dim(NULL, dims, 1.0, prod(dims)),
    TRUE
  ) |> errorfun()
  expect_equal(
    .C_check_conf_dim(rep(1L, i), dims, 1.0, prod(dims)),
    TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
}


# vector by vector ====
expect_equal(
  .C_check_conf_dim(NULL, NULL, 10L, 10L),
  TRUE
)
expect_equal(
  .C_check_conf_dim(NULL, NULL, 10L, 5L),
  FALSE
)
expect_equal(
  .C_check_conf_dim(10L, 10L, 10L, 10L),
  TRUE
)
expect_equal(
  .C_check_conf_dim(10L, 5L, 10L, 5L),
  FALSE
)
enumerate <- enumerate + 4L


# vector by array ====
expect_equal(
  .C_check_conf_dim(c(10L, 10L), c(10L, 1L), 100.0, 10.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(c(10L, 1L), c(10L, 10L), 10.0, 100.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(c(10L, 1L), c(5L, 1L), 100.0, 5.0),
  FALSE
)
expect_equal(
  .C_check_conf_dim(c(5L, 1L), c(10L, 1L), 5.0, 100.0),
  FALSE
)
enumerate <- enumerate + 4L


# ortho vectors ====
expect_equal(
  .C_check_conf_dim(c(10L, 1L), c(1L, 10L), 10.0, 10.0),
  TRUE
)
enumerate <- enumerate + 1L


# arrays by arrays ====
expect_equal(
  .C_check_conf_dim(c(10L, 10L, 1L), c(10L, 10L, 5L), 100.0, 500.0),
  TRUE
)
expect_equal(
  .C_check_conf_dim(c(10L, 10L, 1L), c(5L, 10L, 5L), 100.0, 250.0),
  FALSE
)
enumerate <- enumerate + 2L

