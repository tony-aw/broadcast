
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dimnames <- function(n) {
  lapply(n, \(i)sample(letters, i))
}



# test comnames, rbind ====
x <- matrix(
  rnorm(5), 5, 5, dimnames = test_make_dimnames(c(5, 5))
)
y <- matrix(
  rnorm(5), 5, 5, dimnames = test_make_dimnames(c(5, 5))
)
z <- 1:5
names(z) <- letters[1:5]
input <- list(x, y, z)
expected <- rbind(x, y, z) |> unname()
for(i in 1:2) {
  dimnames(expected)[2L] <- dimnames(input[[i]])[2L]
  expect_equal(
    bind_mat(input, 1L, name_deparse = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  
  enumerate <- enumerate + 1L
}
i = 3
dimnames(expected)[[2L]] <- names(input[[i]])
expect_equal(
  bind_mat(input, 1L, name_deparse = FALSE, comnames_from = i),
  expected
) |> errorfun()
expected <- unname(expected)

enumerate <- enumerate + 1L



# test comnames, cbind ====
x <- matrix(
  rnorm(5), 5, 5, dimnames = test_make_dimnames(c(5, 5))
)
y <- matrix(
  rnorm(5), 5, 5, dimnames = test_make_dimnames(c(5, 5))
)
z <- 1:5
names(z) <- letters[1:5]
input <- list(x, y, z)
expected <- cbind(x, y, z) |> unname()
for(i in 1:2) {
  dimnames(expected)[1L] <- dimnames(input[[i]])[1L]
  expect_equal(
    bind_mat(input, 2L, name_deparse = FALSE, comnames_from = i),
    expected
  ) |> errorfun()
  expected <- unname(expected)
  
  enumerate <- enumerate + 1L
}
i = 3
dimnames(expected)[[1L]] <- names(input[[i]])
expect_equal(
  bind_mat(input, 2L, name_deparse = FALSE, comnames_from = i),
  expected
) |> errorfun()
expected <- unname(expected)

enumerate <- enumerate + 1L


