
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# arrangement = 0 ====
x.data <- list(
  sample(as.raw(0:255)),
  sample(c(TRUE, FALSE, NA)),
  sample(c(-10:10, NA)),
  sample(c(rnorm(20), NA, NaN, Inf, -Inf)),
  sample(c(rnorm(20), NA, NaN, Inf, -Inf)) + sample(c(rnorm(20), NA, NaN, Inf, -Inf)) * -1i,
  sample(c(LETTERS, month.abb, NA))
) |> rep(each = 2)


for(i in seq_along(x.data)) {
  for(j in seq(1, 24, 2)) {
    x <- x.data[sample(1:i, j, TRUE)]
    expect_equal(
      unlist(x),
      cast_shallow2atomic(x)
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
  
}

x <- c(x.data, ~ foo)
expected <- unlist(lapply(x, as.character))
out <- cast_shallow2atomic(x)
expect_equal(
  expected,
  out
)
enumerate <- enumerate + 1L


# arrangement = 1 or -1 ====


x.data <- list(
  sample(as.raw(0:255), 10L, TRUE),
  sample(c(TRUE, FALSE, NA), 10L, TRUE),
  sample(c(-10:10, NA), 10L, TRUE),
  sample(c(rnorm(20), NA, NaN, Inf, -Inf), 10L, TRUE),
  sample(c(rnorm(20), NA, NaN, Inf, -Inf), 10L, TRUE) + sample(c(rnorm(20), NA, NaN, Inf, -Inf), 10L, TRUE) * -1i,
  sample(c(LETTERS, month.abb, NA), 10L, TRUE)
) |> rep(each = 2)

for(i in seq_along(x.data)) {
  for(j in seq(1, 24, 2)) {
    x <- x.data[sample(1:i, j, TRUE)]
    expect_equal(
      cast_shallow2atomic(x, 1L),
      simplify2array(x)
    ) |> errorfun()
    
    expect_equal(
      cast_shallow2atomic(x, -1L),
      simplify2array(x) |> t()
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
    
  }
}

