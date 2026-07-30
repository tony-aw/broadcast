

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# main tests ====

x <- list(
  y = list(
    as.list(setNames(1:26, letters)),
    as.list(setNames(1:25, LETTERS[1:25]))
  ),
  z = list(
    as.list(setNames(1:25, letters[1:25])),
    as.list(setNames(1:26, LETTERS))
  ),
  x = list(
    as.list(setNames(1:25, letters[1:25])),
    as.list(setNames(1:26, LETTERS))
  )
)

expect_equal(
  cast_hier2dim(x, direction.names = 1) |> dimnames(),
  list(letters, NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, direction.names = 1, in2out = FALSE) |> dimnames(),
  list(names(x), NULL, letters)
)

expect_equal(
  cast_hier2dim(x, direction.names = -1) |> dimnames(),
  list(LETTERS, NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, direction.names = -1, in2out = FALSE) |> dimnames(),
  list(names(x), NULL, LETTERS)
)
enumerate <- enumerate + 4L


# recurse through data.frames ====

x <- list(
  x = list(
    as.data.frame(as.list(month.abb)),
    as.data.frame(as.list(month.name))
  ),
  y = list(
    as.list(setNames(1:12, letters[1:12])),
    as.list(setNames(1:12, LETTERS[1:12]))
  ),
  z = list(
    as.list(setNames(1:12, letters[1:12])),
    as.list(setNames(1:12, LETTERS[1:12]))
  ),
  x2 = list(
    as.data.frame(as.list(month.abb)),
    as.data.frame(as.list(month.name))
  )
)

expect_equal(
  cast_hier2dim(x, recurse_all = TRUE, direction.names = 1) |> dimnames(),
  list(paste("X.", month.abb, ".", sep=""), NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, recurse_all = TRUE, direction.names = 1) |> dimnames(),
  list(paste("X.", month.abb, ".", sep=""), NULL, names(x)) |> rev()
)

expect_equal(
  cast_hier2dim(x, direction.names = -1, recurse_all = TRUE) |> dimnames(),
  list(paste("X.", month.name, ".", sep=""), NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, direction.names = -1, in2out = FALSE, recurse_all = TRUE) |> dimnames(),
  list(paste("X.", month.name, ".", sep=""), NULL, names(x)) |> rev()
)
enumerate <- enumerate + 4L


# recurse through recursive arrays ====

x <- list(
  x = list(
    array(as.list(1:12), dimnames = list(month.abb)),
    array(as.list(1:12), dimnames = list(month.name))
  ),
  y = list(
    as.list(setNames(1:12, letters[1:12])),
    as.list(setNames(1:12, LETTERS[1:12]))
  ),
  z = list(
    as.list(setNames(1:12, letters[1:12])),
    as.list(setNames(1:12, LETTERS[1:12]))
  ),
  x2 = list(
    array(as.list(1:12), dimnames = list(month.abb)),
    array(as.list(1:12), dimnames = list(month.name))
  )
)


expect_equal(
  cast_hier2dim(x, recurse_all = TRUE, direction.names = 1) |> dimnames(),
  list(month.abb, NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, recurse_all = TRUE, direction.names = 1) |> dimnames(),
  list(month.abb, NULL, names(x)) |> rev()
)

expect_equal(
  cast_hier2dim(x, direction.names = -1, recurse_all = TRUE) |> dimnames(),
  list(month.name, NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, direction.names = -1, in2out = FALSE, recurse_all = TRUE) |> dimnames(),
  list(month.name, NULL, names(x)) |> rev()
)
enumerate <- enumerate + 4L

