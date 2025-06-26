
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

vc <- datasets::ability.cov$cov
expect_equal(
  chol2inv(chol(vc)),
  cinv(vc)
)
enumerate <- 1L
