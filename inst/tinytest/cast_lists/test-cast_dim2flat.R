
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# unnamed array ====

x <- array(as.list(1:60), 3:5)
out <- cast_dim2flat(x)
for(i in sample(1:nrow(x))) {
  for(j in sample(1:ncol(x))) {
    for(k in sample(1:dim(x)[3])) {
      name <- paste0("[", i, ", ", j, ", ", k, "]")
      expect_equal(
        out[[name]],
        x[[i, j, k]]
      ) |> errorfun()
      enumerate <- enumerate + 1L
    }
    
  }
}


# named array ====

x <- array(as.list(1:60), 3:5)
dimnames(x) <- list(letters[1:3], NULL, month.abb[1:5])
out <- cast_dim2flat(x)
for(i in sample(1:nrow(x))) {
  for(j in sample(1:ncol(x))) {
    for(k in sample(1:dim(x)[3])) {
      name <- paste0("['", rownames(x)[i], "', ", j, ", '", dimnames(x)[[3]][k], "']")
      expect_equal(
        out[[name]],
        x[[ rownames(x)[i], j, dimnames(x)[[3]][k] ]]
      ) |> errorfun()
      enumerate <- enumerate + 1L
    }
    
  }
}


# error tests ====

expect_error(
  cast_dim2flat(as.list(1:10)),
  pattern = "`x` has no dimensions"
)
x <- list()
dim(x) <- c(0, 10)
expect_error(
  cast_dim2flat(x),
  pattern = "`x` has zero-length dimensions"
)
x <- array(as.list(1:10), 10L)
expect_error(
  cast_dim2flat(x),
  pattern = "`x` is single-dimensional"
)
x <- array(as.list(1:10), rep(2, 17))
expect_error(
  cast_dim2flat(x),
  pattern = "arrays with more than 16 dimensions not supported"
)

enumerate <- enumerate + 4L

