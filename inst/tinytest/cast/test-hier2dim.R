
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist
.rcpp_clone <- broadcast:::.rcpp_clone
.rcpp_hier_flatlen <- broadcast:::.rcpp_hier_flatlen
.hiercast_depth <- broadcast:::.hiercast_depth


# main function test ====
for(i in 2:16) {
  for(j in 1:10) {
    lens <- sample(1:3, i, TRUE)
    x <- .rcpp_allocate_nestedlist(lens, 1)
    expect_equal(
      hier2dim(x),
      rev(lens)
    ) |> errorfun()
    expect_equal(
      hier2dim(x, FALSE),
      lens
    ) |> errorfun()
    enumerate <- enumerate + 2L
  }
}


for(i in 2:16) {
  for(j in 1:10) {
    lens <- sample(1:3, i, TRUE)
    x <- .rcpp_allocate_nestedlist(lens, 1)
    
    out <- cast_hier2dim(x)
    expect_equal(
      dim(out),
      rev(lens)
    ) |> errorfun()
    
    out <- cast_hier2dim(x, FALSE)
    expect_equal(
      dim(out),
      lens
    ) |> errorfun()
    enumerate <- enumerate + 2L
  }
}



# pass-by-reference safety checks ====


x <- list(
  list(as.list(1:10)),
  list(as.list(1:9)),
  list(1:11),
  list(list(NULL)),
  list(list()),
  data.frame(letters, LETTERS)
)
y <- .rcpp_clone(x)

out <- .rcpp_hier_flatlen(x, 16L, FALSE)
out2 <- .rcpp_hier_flatlen(x, 16L, FALSE)

expect_equal(
  out, out2
)
expect_equal(
  x, y
)


x <- list(
  list(as.list(1:10)),
  list(as.list(1:9)),
  list(list(1:11)),
  list(list()),
  list(list(NULL)),
  list(data.frame(letters, LETTERS))
)
y <- .rcpp_clone(x)


out <- .hiercast_depth(x, 16L, FALSE, sys.call())
out2 <- .hiercast_depth(x, 16L, FALSE, sys.call())

expect_equal(
  out, out2
)
expect_equal(
  x, y
)

enumerate <- enumerate + 6L


# limit depth tests ====

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  hier2dim(x, maxdepth = 6L),
  rep(1, 6)
)

x <- .rcpp_allocate_nestedlist(rep(1, 20), 1)
expect_equal(
  hier2dim(x, maxdepth = 15),
  rep(1, 15)
)



# errors ====

expect_error(
  hier2dim(c(x, data.frame(letters, LETTERS))),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  hier2dim(rep(list(NULL), 10)),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  hier2dim(list()),
  pattern = "cannot cast zero-length list"
)
expect_error(
  hier2dim(1:10),
  pattern = "`x` must be a list"
)
expect_error(
  hier2dim(x, NA),
  pattern = "`in2out` must be `TRUE` or `FALSE`"
)
expect_error(
  hier2dim(x, recurse_classed = NA),
  pattern = "`recurse_classed` must be `TRUE` or `FALSE`"
)
expect_error(
  hier2dim(matrix(as.list(1:10))),
  pattern = "`x` already has dimensions"
)
expect_error(
  hier2dim(as.list(1:10), maxdepth = NA),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  hier2dim(as.list(1:10), maxdepth = NA_integer_),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  hier2dim(as.list(1:10), maxdepth = 1:10),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)

x <- .rcpp_allocate_nestedlist(rep(2, 18), 1)
expect_equal(
  hier2dim(x),
  rep(2, 16) # 16, NOT 18, BECAUSE I DON'T ALLOW GOING ANY DEEPER THAN 16
)

enumerate <- enumerate + 10L

