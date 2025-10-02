# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- 1:10

funs <- list(
  as_bool,
  as_int,
  as_dbl,
  as_cplx,
  as_chr,
  as_raw,
  as_list
)

for(i in seq_along(funs)) {
  y <- x
  expect_false(broadcaster(funs[[i]](y))) |> errorfun()
  broadcaster(y) <- TRUE
  expect_true(broadcaster(funs[[i]](y))) |> errorfun()
  enumerate <- enumerate + 2L
}

