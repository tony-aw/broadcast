
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

x <- 1:10
y <- matrix(1:20, ncol = 2)
expect_equal(
  cbind(x, y) |> unname(),
  bind_mat(list(x, y), 2L)
)
y <- matrix(1:20, nrow = 2)
expect_equal(
  rbind(x, y) |> unname(),
  bind_mat(list(x, y), 1L)
)
enumerate <- enumerate + 1L

# more tests to add: naming tests...