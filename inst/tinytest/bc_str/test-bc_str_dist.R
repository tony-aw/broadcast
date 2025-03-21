

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 5:
  out <- lapply(1:n, \(n)sample(c(1, 5), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}
.return_missing <- broadcast:::.return_missing



enumerate <- 0L



# basic tests ====

expect_equal(
  bc.str(array("hello"), array("hello"), "levenshtein"),
  array(0L)
)
expect_equal(
  bc.str(array("kitten"), array("sitting"), "levenshtein"),
  array(3L)
)
expect_equal(
  bc.str(array("kitten"), array("kkkitten"), "levenshtein"),
  array(2L)
)
expect_equal(
  bc.str(array("hello"), array("hellok"), "levenshtein"),
  array(1L)
)
expect_equal(
  bc.str(array("helklo"), array("hello"), "levenshtein"),
  array(1L)
)

expect_equal(
  bc.str(month.name, array(month.abb, c(1, 12)), "levenshtein"),
  adist(month.name, month.abb)
)

enumerate <- enumerate + 6L
