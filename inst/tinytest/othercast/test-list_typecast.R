# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

datagens <- list(
  \() sample(as.raw(0:9), 22L, TRUE),
  \() sample(c(TRUE, FALSE, NA), 22L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(18), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(18), NA, NaN, Inf, -Inf)) + sample(c(rnorm(18), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA), 22)
)

# test whole vector type casting ====

for(i in seq_along(datagens)) {
  x <- datagens[[i]]()
  names(x) <- sample(letters, length(x), TRUE)
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  
  out <- as.list(x)
  attributes(out) <- myattr
  expect_equal(as_list(x), out) |> errorfun()
  enumerate <- enumerate + 1L
}


# test partial vector type casting ====

for(i in seq_along(datagens)) {
  x <- datagens[[i]]()
  names(x) <- sample(letters, length(x), TRUE)
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  
  out <- expect <- x
  out[1] <- as_list(out[1])
  expect[1] <- as.list(out[1])
  attributes(expect) <- myattr
  expect_equal(out, expect) |> errorfun()
  enumerate <- enumerate + 1L
}


# test whole matrix type casting ====

for(i in seq_along(datagens)) {
  x <- datagens[[i]]() |> matrix(ncol = 2)
  colnames(x) <- c("one", "two")
  rownames(x) <- month.abb[1:11]
  names(x) <- sample(letters, length(x), TRUE)
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  
  out <- as.list(x)
  attributes(out) <- myattr
  expect_equal(as_list(x), out) |> errorfun()
  enumerate <- enumerate + 1L
}


# test partial matrix type casting ====

# here, attributes shouldn't be preserved
# since we're changing a single element of an atomic matrix
# with a list-elment
for(i in seq_along(datagens)) {
  x <- datagens[[i]]() |> matrix(ncol = 2)
  colnames(x) <- c("one", "two")
  rownames(x) <- month.abb[1:11]
  names(x) <- sample(letters, length(x), TRUE)
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  
  out <- expect <- x
  out[1] <- as_list(out[1])
  expect[1] <- as.list(out[1])
  expect_equal(out, expect) |> errorfun()
  enumerate <- enumerate + 1L
}

