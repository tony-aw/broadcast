


funs <- list(
  bc.b = bc.b,
  bc.i = bc.i,
  bc.d = bc.d,
  bc.cplx = bc.cplx,
  bc.str = bc.str,
  bc.raw = bc.raw,
  bc.bit = bc.bit,
  bc.rel = bc.rel,
  bc.list = bc.list
)
funs <- c(funs, funs, funs, funs)
ops1 <- c( # relops
  rep(list("=="), 8L),
  \(x, y) length(x)==length(y)
)
ops2 <- c( # main ops
  list("&"), list("%/%"), rep(list("+"), 3L), list("pmin", "&"), "!=",
  \(x, y) length(x)==length(y)
)
ops3 <- c( # dist ops
  list("|", "%%", "d==", "/", "levenshtein", "pmin", "&", "!="),
  \(x, y) length(x)==length(y)
)
ops4 <- c( # other ops
  list("|", "%%", "pmax", "/", "==", "pmin", "&", "!="),
  \(x, y) length(x)==length(y)
)
ops <- c(ops1, ops2, ops3, ops4)

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(0:255, 10)),
  \() as.raw(sample(0:255, 10)),
  \() as.raw(sample(0:255, 10)),
  \() sample(list(letters, month.abb, 1:10, NULL))
)
datagens <- c(datagens, datagens, datagens, datagens)

.rcpp_address <- broadcast:::.rcpp_address
.rcpp_clone <- broadcast:::.rcpp_clone

