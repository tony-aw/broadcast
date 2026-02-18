
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.C_seq_Clen <- broadcast:::.C_seq_Clen
.C_make_dcp <- broadcast:::.C_make_dcp


expect_equal(
  seq(1, 10),
  .C_seq_Clen(1, 10)
)

expect_equal(
  .C_make_dcp(1:10),
  c(1, cumprod(1:10))
)



enumerate <- enumerate + 2L

