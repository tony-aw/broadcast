
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
normalizefun <- function(x, n) {
  length(x) <- n
  return(x)
}


# recursive vector ====
x <- list(
  setNames(1:11, letters[1:11]), 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
)
names(x) <- month.abb

expect_equal(
  cast_shallow2atomic(x, 0L),
  unlist(x)
)


expect_equal(
  cast_shallow2atomic(x, 1L, comnames_from = 1L),
  simplify2array(lapply(x, normalizefun, n = 11))
)

expect_equal(
  cast_shallow2atomic(x, -1L, comnames_from = 1L),
  simplify2array(lapply(x, normalizefun, n = 11)) |> t()
)
enumerate <- enumerate + 3L



# recursive matrix ====
x <- list(
  setNames(1:11, letters[1:11]), 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
) |> rev()
dim(x) <- c(3, 4)
dimnames(x) <- list(month.abb[1:3], month.name[1:4])


expect_equal(
  cast_shallow2atomic(x, 0L),
  unlist(x)
)

expected <- simplify2array(lapply(x, normalizefun, n = 11))
dim(expected) <- c(11L, 3L, 4L)
dimnames(expected) <- c(list(letters[1:11]), dimnames(x))

expect_equal(
  cast_shallow2atomic(x, 1L, comnames_from = length(x)),
  expected
)

expected <- lapply(x, normalizefun, n = 11)
attributes(expected) <- attributes(x)
out <- integer(11 * length(expected))
dim(out) <- c(dim(expected), 11L)
dimnames(out) <- c(dimnames(expected), list(letters[1:11]))

for(i in 1:nrow(expected)) {
  for(j in 1:ncol(expected)) {
    out[i, j, ] <- expected[[i, j]]
  }
}
expected <- out

expect_equal(
  cast_shallow2atomic(x, -1L, comnames_from = length(x)),
  expected
)

enumerate <- enumerate + 3L


