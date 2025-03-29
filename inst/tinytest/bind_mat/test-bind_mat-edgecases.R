
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# bind empty with non-empty ====
x <- array(1:12, c(12, 1))
names(x) <- month.abb
emptyarray <- array(numeric(0L), c(0L, 10))
input <- list(emptyarray, x)
for(i in 1:2) {
  expect_equal(
    bind_mat(input, i),
    x
  ) |> errorfun()
}
enumerate <- enumerate + 2L



# bind with empty or NA input names ====

x <- array(1:20, c(5, 3), list(NULL, LETTERS[1:3]))
y <- array(-1:-20, c(5, 3))
z <- array(-1:-20, c(5, 3))
input <- list(x, y, z)
names(input) <- c("", NA, "")

expected <- do.call(cbind, input)
expect_equal(
  bind_mat(input, 2L),
  expected
)
expect_equal(
  bind_mat(input, 2L),
  expected
)

enumerate <- enumerate + 2L
