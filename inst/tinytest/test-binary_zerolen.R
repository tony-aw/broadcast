# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

funs <- list(
  bc.b,
  bc.i,
  bc.d,
  bc.cplx,
  bc.str,
  bc.raw,
  bc.bit
)

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(0:255, 10)),
  \() as.raw(sample(0:255, 10))
)


# relational operators ====

expected <- logical(0L)
counter <- 1L
for(i in 1:6) {
  for(xLen in c(10, 0)) {
    for(yLen in c(0, 10)) {
      
      # prep data:
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      if(xLen == 0L) {
        x <- x[0L]
      }
      else if(yLen == 0L) {
        y <- y[0L]
      }
      else {
        x <- x[0L]
        y <- y[0L]
      }
      
      expect_equal(
        funs[[i]](x, y, "=="),
        expected
      ) |> errorfun()
      enumerate <- enumerate + 1L
      
    }
  }
}


# main operators ====

ops <- c(
  "&", rep("+", 4), "diff", "&"
)

for(i in seq_along(funs)) {
  for(xLen in c(10, 0)) {
    for(yLen in c(0, 10)) {
      
      # prep data:
      x <- datagens[[i]]()
      y <- datagens[[i]]()
      
      if(xLen == 0L) {
        x <- x[0L]
      }
      else if(yLen == 0L) {
        y <- y[0L]
      }
      else {
        x <- x[0L]
        y <- y[0L]
      }
      
      expected <- vector(typeof(x), 0L)
      
      expect_equal(
        funs[[i]](x, y, ops[i]),
        expected
      ) |> errorfun()
      enumerate <- enumerate + 1L
      
    }
  }
}


# special operators ====
x <- letters
y <- character(0L)
expect_equal(
  bc.str(x, y, "levenshtein"),
  integer(0L)
)
expect_equal(
  bc.str(y, x, "levenshtein"),
  integer(0L)
)
x <- as.list(x)
y <- vector("list", 0L)
expect_equal(
  bc.list(x, y, \(x, y) paste0(x, y)),
  vector("list", 0L)
)
expect_equal(
  bc.list(y, x, \(x, y) paste0(x, y)),
  vector("list", 0L)
)

enumerate <- enumerate + 4L


