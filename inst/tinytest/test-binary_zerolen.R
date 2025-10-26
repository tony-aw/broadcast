# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
source(file.path(getwd(), "source.R"))

# relational operators ====

expected <- logical(0L)
counter <- 1L
for(i in 1:5) {
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

for(i in seq_along(ops)) {
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


