


funs <- list(
  bc.b = bc.b,
  bc.i = bc.i,
  bc.d = bc.d,
  bc.cplx = bc.cplx,
  bc.str = bc.str,
  bc.raw = bc.raw,
  bc.bit = bc.bit,
  bc.rel = bc.rel,
  bc_strrep = \(x, y, op) {bc_strrep(as_chr(x), y)},
  bc.list = bc.list
)
funs <- c(funs, funs)
ops1 <- c(
  rep(list("=="), 8L), "",
  \(x, y) length(x)==length(y)
)
ops2 <- c(
  list("&"), rep(list("+"), 4L), list("pmin", "&"), "!=", "",
  \(x, y) length(x)==length(y)
)
ops <- c(ops1, ops2)

datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() as.raw(sample(0:255, 10)),
  \() as.raw(sample(0:255, 10)),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(list(letters, month.abb, 1:10, NULL))
)
datagens <- c(datagens, datagens)

