
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dimnames <- function(n) {
  lapply(n, \(i)sample(letters, i))
}


x <- setNames(1:10, month.abb[1:10])
y <- matrix(1:20, ncol = 2, dimnames = list(letters[1:10], LETTERS[1:2]))
z <- matrix(21:40, ncol = 2, dimnames = list(letters[11:20], LETTERS[3:4]))
expect_equal(
  cbind(x, y, z) |> unname(),
  bind_mat(list(x, y, z), 2L, name_deparse = FALSE, comnames_from = NULL)
)

x <- setNames(1:2, month.name[1:2])
expect_equal(
  rbind(x, y, z) |> unname(),
  bind_mat(list(x, y, z), 1L, name_deparse = FALSE, comnames_from = NULL)
)
enumerate <- enumerate + 2L


