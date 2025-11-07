# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

coercionfuns <- list(
  as.logical,
  as.integer,
  as.double,
  as.complex,
  as.character,
  as.raw,
  as.list
)

# return unchanged ====
for(iType in seq_along(coercionfuns)) {
  for(iNdim in 1:3) {
    x <- array(coercionfuns[[iType]](1:27), sample(1:10, iNdim, TRUE))
    expect_equal(
      vector2array(x, 1, 1),
      x
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
  
}

# no names ====
for(iType in seq_along(coercionfuns)) {
  for(iDir in 1:3) {
    for(iNdim in iDir:(iDir+3)) {
      for(iLen in seq(1, 20, 2)) {
        x <- coercionfuns[[iType]](1:iLen)
        x.dim <- rep(1L, iNdim)
        x.dim[iDir] <- length(x)
        expect_equal(
          vector2array(x, iDir, iNdim),
          array(x, dim = x.dim)
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
      }
    }
  }
}



# with names ====
for(iType in seq_along(coercionfuns)) {
  for(iDir in 1:3) {
    for(iNdim in iDir:(iDir+3)) {
      for(iLen in seq(1, 20, 2)) {
        x <- setNames(coercionfuns[[iType]](1:iLen), sample(letters, iLen))
        x.dim <- rep(1L, iNdim)
        x.dim[iDir] <- length(x)
        x.dimnames <- rep(list(NULL), iNdim)
        x.dimnames[[iDir]] <- names(x)
        expect_equal(
          vector2array(x, iDir, iNdim),
          array(x, dim = x.dim, dimnames = x.dimnames)
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
      }
    }
  }
}

# broadcaster ====
x <- 1:10
broadcaster(x) <- TRUE
expect_equal(
  vector2array(x, 1, 2) |> broadcaster(),
  broadcaster(x)
)

broadcaster(x) <- FALSE
expect_equal(
  vector2array(x, 1, 2) |> broadcaster(),
  broadcaster(x)
)

x <- 1:10
broadcaster(x) <- TRUE
expect_equal(
  vector2array(x, 1, 2, FALSE) |> broadcaster(),
  FALSE
)

broadcaster(x) <- FALSE
expect_equal(
  vector2array(x, 1, 2, TRUE) |> broadcaster(),
  TRUE
)

enumerate <- enumerate + 4L



# errors ====
x <- setNames(1:19, sample(letters, 19))
for(iDir in c(NA, -1, 0)) {
  expect_error(
    vector2array(x, NA, 10L),
    pattern = "`direction` must be a strictly positive integer scalar"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}
for(iNdim in c(NA, -1, 0)) {
  expect_error(
    vector2array(x, 1L, iNdim),
    pattern = "`ndim` must be a strictly positive integer scalar"
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

expect_error(
  vector2array(x, 2L, 1L),
  pattern = "`ndim` must be a strictly positive integer scalar, and `>= direction` and `<= 16`"
)
expect_error(
  vector2array(~ foo, 1L, 2L),
  pattern = "`x` must be atomic or a list"
)
enumerate <- enumerate + 1L



