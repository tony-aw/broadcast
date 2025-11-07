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

# no names ====
for(iType in seq_along(coercionfuns)) {
  for(iDir in 1:3) {
    for(iNdim in iDir:(iDir+3)) {
      for(iLen in seq(1, 20, 2)) {
        x <- coercionfuns[[iType]](1:iLen)
        x.dim <- rep(1L, iNdim)
        x.dim[iDir] <- length(x)
        expect_equal(
          vector2array(x, iDir, iNdim) |> array2vector(),
          x
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
          vector2array(x, iDir, iNdim) |> array2vector(),
          x
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
  vector2array(x, 1, 2) |> array2vector() |> broadcaster(),
  broadcaster(x)
)

broadcaster(x) <- FALSE
expect_equal(
  vector2array(x, 1, 2) |> array2vector() |> broadcaster(),
  broadcaster(x)
)


enumerate <- enumerate + 2L


