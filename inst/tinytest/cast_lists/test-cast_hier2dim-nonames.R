
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist


# no names at all + correct length tests ====
for(iN in 2:16) {
  for(iOut in c(TRUE, FALSE)) {
    for(iDir in c(1, -1)) {
      for(j in 1:5) {
        lens <- sample(1:3, iN, TRUE)
        x <- .rcpp_allocate_nestedlist(lens, 1)
        expect_equal(
          cast_hier2dim(x, in2out = iOut, direction.names = iDir) |> dimnames(),
          NULL
        ) |> errorfun()
        
        expect_equal(
          cast_hier2dim(x, in2out = iOut, direction.names = iDir, maxdepth = 8L) |> dimnames(),
            NULL
        ) |> errorfun()
        
        enumerate <- enumerate + 2L
        
      }
    }
  }
}


# no valid names test ====
x <- list(
  list(
    class2 = list(
      rnorm(10, 170),
      rnorm(10, 80),
      sample(c("M", "F", NA), 10, TRUE)
    )
  ),
  list(
    list(
      height = rnorm(10, 170),
      weight = rnorm(10, 80)
    ),
    list(
      rnorm(10, 170),
      rnorm(10, 80),
      sample(c("M", "F", NA), 10, TRUE)
    )
  )
)
for(iOut in c(TRUE, FALSE)) {
  for(iDir in c(1, -1)) {
    expect_equal(
      cast_hier2dim(x, in2out = iOut, direction.names = iDir) |> dimnames(),
      NULL
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}



# no nested names ====
x <- list(
  x = as.list(1:10),
  y = as.list(letters),
  z = as.list(month.abb)
)
expect_equal(
  cast_hier2dim(x, direction.names = 1) |> dimnames(),
  list(NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, direction.names = -1) |> dimnames(),
  list(NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, direction.names = 1) |> dimnames(),
  list(names(x), NULL)
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, direction.names = -1) |> dimnames(),
  list(names(x), NULL)
)

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
  cast_hier2dim(x, direction.names = 1) |> dimnames(),
  list(NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, direction.names = -1) |> dimnames(),
  list(NULL, names(x))
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, direction.names = 1) |> dimnames(),
  list(names(x), NULL)
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, direction.names = -1) |> dimnames(),
  list(names(x), NULL)
)

enumerate <- enumerate + 8L



# no surface names; only nested names of unequal lengths ====
x <- list(
  as.list(setNames(1:10, LETTERS[1:10])),
  as.list(setNames(letters, letters)),
  as.list(setNames(month.abb, month.abb)),
  as.list(setNames(letters, LETTERS))
)
expect_equal(
  cast_hier2dim(x, direction.names = 1) |> dimnames(),
  list(letters, NULL)
)
expect_equal(
  cast_hier2dim(x, direction.names = -1) |> dimnames(),
  list(LETTERS, NULL)
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, direction.names = 1) |> dimnames(),
  list(NULL, letters)
)
expect_equal(
  cast_hier2dim(x, in2out = FALSE, direction.names = -1) |> dimnames(),
  list(NULL, LETTERS)
)

enumerate <- enumerate + 4L


# names but direction = 0 ====

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
  cast_hier2dim(x, direction.names = 0) |> dimnames(),
  NULL
)

enumerate <- enumerate + 1L
