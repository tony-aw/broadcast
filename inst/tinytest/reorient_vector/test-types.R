# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x.data <- list(
  as.raw(0:9),
  sample(c(TRUE, FALSE, NA), 10, TRUE),
  c(1:9, NA),
  c(rnorm(6), NA, NaN, Inf, -Inf),
  c(rnorm(6), NA, NaN, Inf, -Inf) + c(rnorm(6), NA, NaN, Inf, -Inf) * -1i,
  c(letters[1:9], NA)
)
x.data[[7L]] <- c(x.data, ~ foo, list(NULL), ~ foo, list(NULL))

for(i in seq_along(x.data)) {
  
  x <- setNames(x.data[[i]], month.abb[1:10])
  x2 <- x
  x %orientbc<-% 1L
  expect_true(
    broadcaster(x)
  ) |> errorfun()
  expect_true(
    is.array(x)
  ) |> errorfun()
  expect_equal(
    dim(x), length(x)
  ) |> errorfun()
  expect_equal(
    dimnames(x)[[1L]], names(x2)
  ) |> errorfun()
  attributes(x) <- NULL
  attributes(x2) <- NULL
  expect_equal(
    x, x2
  ) |> errorfun()
  
  x <- setNames(x.data[[i]], month.abb[1:10])
  x2 <- x
  x %orientbc<-% 2L
  expect_true(
    broadcaster(x)
  ) |> errorfun()
  expect_true(
    is.array(x)
  ) |> errorfun()
  expect_equal(
    dim(x), c(1L, length(x))
  ) |> errorfun()
  expect_equal(
    dimnames(x)[[2L]], names(x2)
  ) |> errorfun()
  attributes(x) <- NULL
  attributes(x2) <- NULL
  expect_equal(
    x, x2
  ) |> errorfun()
  
  
  x <- setNames(x.data[[i]], month.abb[1:10])
  x2 <- x
  x %orientbc<-% 3L
  expect_true(
    broadcaster(x)
  ) |> errorfun()
  expect_true(
    is.array(x)
  ) |> errorfun()
  expect_equal(
    dim(x), c(1L, 1L, length(x))
  ) |> errorfun()
  expect_equal(
    dimnames(x)[[3L]], names(x2)
  ) |> errorfun()
  attributes(x) <- NULL
  attributes(x2) <- NULL
  expect_equal(
    x, x2
  ) |> errorfun()
  
  enumerate <- enumerate + 5L*3L
}


