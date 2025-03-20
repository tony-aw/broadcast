
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

