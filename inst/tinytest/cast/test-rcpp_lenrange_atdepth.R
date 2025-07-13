

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist
.rcpp_clone <- broadcast:::.rcpp_clone
.rcpp_lenrange_atdepth <- broadcast:::.rcpp_lenrange_atdepth


# main check ====

for(i in 1:5) {
  for(iSample in 1:10) {
    lens <- sample(2:5, i+1, TRUE)
    x <- .rcpp_allocate_nestedlist(lens, 1)
    x[[rep(1, i+1)]] <- NULL
    out <- .rcpp_lenrange_atdepth(x, i, FALSE)
    expect_equal(
      out,
      range(lens[i+1] - 1, lens[i+1])
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}


# consistency check ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  data.frame(letters)
)

out <- .rcpp_lenrange_atdepth(x, 2L, FALSE)
out2 <- .rcpp_lenrange_atdepth(x, 2L, FALSE)

expect_equal(
  out, out2
)
enumerate <- enumerate + 1L



# unit list ====
x <- list(list(list(list(NULL))))
expect_equal(
  .rcpp_lenrange_atdepth(x, 1L, FALSE),
  c(1L, 1L)
)

x <- list(list(list(list(data.frame(letters, LETTERS)))))
expect_equal(
  .rcpp_lenrange_atdepth(x, 4L, TRUE),
  c(2L, 2L)
)

enumerate <- enumerate + 2L


# recursive vector ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  list(list(~ hello)),
  data.frame(letters)
)
expect_equal(
  .rcpp_lenrange_atdepth(x, 1L, FALSE),
  c(1L, 10L)
)
expect_equal(
  .rcpp_lenrange_atdepth(x, 2L, recurse_classed = TRUE),
  c(1L, 1L)
)

enumerate <- enumerate + 2L


# pass-by-reference safety checks ====

x <- list(
  list(list(list(list(1L)))),
  as.list(1:10),
  list(list(list())),
  list(list(NULL)),
  data.frame(letters)
)
y <- .rcpp_clone(x)

out <- .rcpp_lenrange_atdepth(x, 1L, FALSE)
out2 <- .rcpp_lenrange_atdepth(x, 1L, FALSE)

expect_equal(
  out, out2
)
expect_equal(
  x, y
)

enumerate <- enumerate + 2L


