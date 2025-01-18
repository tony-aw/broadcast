# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops


# test whole vector type casting ====
x <- seq(-2.5, 2, by =0.5)
names(x) <- letters[1:10]
attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- as.logical(x)
attributes(out) <- myattr
expect_equal(as_bool(x), out)

out <- as.integer(x)
attributes(out) <- myattr
expect_equal(as_int(x), out)

out <- as.double(x)
attributes(out) <- myattr
expect_equal(as_dbl(x), out)

out <- as.character(x)
attributes(out) <- myattr
expect_equal(as_chr(x), out)

x <- 1:10
names(x) <- letters[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- as.complex(x)
attributes(out) <- myattr
expect_equal(as_cplx(x), out)

out <- as.raw(x)
attributes(out) <- myattr
expect_equal(as_raw(x), out)

enumerate <- enumerate + 6L


# test partial vector type casting ====
x <- seq(-2.5, 2, by =0.5)
names(x) <- letters[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- expect <- x
out[1] <- as_bool(out[1])
expect[1] <- as.logical(out[1])
expect_equal(out, expect)

out <- expect <- x
out[1] <- as_int(out[1])
expect[1] <- as.integer(out[1])
expect_equal(out, expect)

out <- expect <- x
out[1] <- as_dbl(out[1])
expect[1] <- as.double(out[1])
expect_equal(out, expect)

out <- expect <- x
out[1] <- as_chr(out[1])
expect[1] <- as.character(out[1])
expect_equal(out, expect)

x <- 1:10
names(x) <- LETTERS[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- expect <- x
out[1] <- as_cplx(out[1])
expect[1] <- as.complex(out[1])
expect_equal(out, expect)

enumerate <- enumerate + 5L
# not doing raw here, as subassignment to/from raw is not supported.



# test vector chained type casting for all combos ====
initials <- list(
  sample(c(TRUE, FALSE, size=10, replace=TRUE)),
  1L:10L, seq(-2.5, 2, by =0.5), as.character(1:10)
)
as.funs1 <- as.funs2 <- list(as.logical, as.integer, as.double, as.character, as.complex, as.raw)
tiny.funs1 <- tiny.funs2 <- list(as_bool, as_int, as_dbl, as_chr, as_cplx, as_raw)
outs <- list()
expects<- list()
iter <- 1
loops <- loops + 1
for (i in 1:4) {
  x <- initials[[i]]
  names(x) <- letters[1:length(x)]
  
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  for (j in 1:4) {
    for (k in 1:4) {
      expects[[iter]] <- as.funs1[[j]](as.funs2[[k]](x))
      attributes(expects[[iter]]) <- myattr
      outs[[iter]] <- tiny.funs1[[j]](tiny.funs2[[k]](x))
      iter <- iter + 1
      enumerate <- enumerate + 1
    }
  }
}
expect_equal(outs, expects)


# test whole matrix type casting ====
x <- seq(-2.5, 2, by =0.5) |> matrix(ncol=2)
colnames(x) <- c("one", "two")
rownames(x) <- month.abb[1:5]
names(x) <- letters[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- as.logical(x)
attributes(out) <- myattr
expect_equal(as_bool(x), out)

out <- as.integer(x)
attributes(out) <- myattr
expect_equal(as_int(x), out)

out <- as.double(x)
attributes(out) <- myattr
expect_equal(as_dbl(x), out)

out <- as.character(x)
attributes(out) <- myattr
expect_equal(as_chr(x), out)

x <- matrix(1:10, ncol=2)
colnames(x) <- c("one", "two")
rownames(x) <- month.abb[1:5]
names(x) <- letters[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- as.complex(x)
attributes(out) <- myattr
expect_equal(as_cplx(x), out)

out <- as.raw(x)
attributes(out) <- myattr
expect_equal(as_raw(x), out)

enumerate <- enumerate + 6L


# test partial matrix type casting ====
x <- seq(-2.5, 2, by =0.5) |> matrix(ncol=2)
colnames(x) <- c("one", "two")
rownames(x) <- month.abb[1:5]
names(x) <- letters[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL

out <- expect <- x
out[, 1] <- as_bool(out[, 1])
expect[, 1] <- as.logical(out[, 1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_int(out[,1])
expect[, 1] <- as.integer(out[,1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_dbl(out[, 1])
expect[, 1] <- as.double(out[, 1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_chr(out[, 1])
expect[, 1] <- as.character(out[, 1])
expect_equal(out, expect)

x <- matrix(1:10, ncol=2)
colnames(x) <- c("one", "two")
rownames(x) <- month.abb[1:5]
names(x) <- letters[1:10]

attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL
out <- expect <- x

out[, 1] <- as_cplx(out[, 1])
expect[, 1] <- as.complex(out[, 1])
expect_equal(out, expect)

enumerate <- enumerate + 5L


# test matrix chained type casting for all combos ====
initials <- list(
  sample(c(TRUE, FALSE, size=10, replace=TRUE)),
  1L:10L, seq(-2.5, 2, by =0.5), as.character(1:10)
)
as.funs1 <- as.funs2 <- list(as.logical, as.integer, as.double, as.character, as.complex, as.raw)
tiny.funs1 <- tiny.funs2 <- list(as_bool, as_int, as_dbl, as_chr, as_cplx, as_raw)
outs <- list()
expects<- list()
iter <- 1
loops <- loops + 1
for (i in 1:4) {
  x <- initials[[i]] |> matrix(ncol=2)
  colnames(x) <- c("one", "two")
  rownames(x) <- month.abb[1:nrow(x)]
  names(x) <- letters[1:length(x)]
  
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  for (j in 1:4) {
    for (k in 1:4) {
      expects[[iter]] <- as.funs1[[j]](as.funs2[[k]](x))
      attributes(expects[[iter]]) <- myattr
      outs[[iter]] <- tiny.funs1[[j]](tiny.funs2[[k]](x))
      iter <- iter + 1
      enumerate <- enumerate + 1
    }
  }
}
expect_equal(outs, expects)

