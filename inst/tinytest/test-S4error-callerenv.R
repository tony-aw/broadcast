# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}



funs <- list(
  bc.b = bc.b,
  bc.i = bc.i,
  bc.d = bc.d,
  bc.cplx = bc.cplx,
  bc.str = bc.str,
  bc.raw = bc.raw,
  bc.bit = bc.bit,
  bc.rel = bc.rel,
  bc.list = bc.list,
  bc_ifelse = bc_ifelse,
  bcapply = bcapply
)
ops <- c(
  rep(list("=="), 7L),
  \(x, y) x == y
)

for(i in seq_along(funs)) {
  output <- tryCatch(funs[[i]](1:10, 1:11, "&"), error = function(e)print(e))
  expect_equal(
    output$call,
    names(funs)[i]
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


