
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# bind_array as c() ====
x <- array(sample(1:10))
y <- array(sample(1:10, 1L))
names(x) <- letters[1:10]
names(y) <- "a"
out <- c(x, y)
expect_equal(
  as.array(out),
  bind_array(list(x, y), 1L)
)


# bind empty with non-empty ====
x <- array(1:12, c(12, 1))
names(x) <- month.abb
emptyarray <- array(numeric(0L), c(0L, 1:10))
input <- list(emptyarray, x)
for(i in 0:3) {
  expect_equal(
    bind_array(input, i),
    x
  ) |> errorfun()
}
enumerate <- enumerate + 4L



# bind with empty or NA input names ====

x <- array(1:20, c(5, 3), list(NULL, LETTERS[1:3]))
y <- array(-1:-20, c(5, 3))
z <- array(-1:-20, c(5, 3))
input <- list(x, y, z)
names(input) <- c("", NA, "")

expected <- do.call(cbind, input)
expect_equal(
  bind_array(input, 2L),
  expected
)
enumerate <- enumerate + 1L
