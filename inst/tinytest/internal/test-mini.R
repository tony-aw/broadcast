
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.C_seq_Clen <- broadcast:::.C_seq_Clen
.C_make_dcp <- broadcast:::.C_make_dcp
.C_dropdims_count <- broadcast:::.C_dropdims_count
.C_dropdims_which <- broadcast:::.C_dropdims_which

expect_equal(
  seq(1, 10),
  .C_seq_Clen(1, 10)
)

expect_equal(
  .C_make_dcp(1:10),
  c(1, cumprod(1:10))
)

x <- sample(1:5, 10, TRUE)
y <- sample(1:5, 10, TRUE)
expect_equal(
  .C_dropdims_count(x, y),
  sum(x == 1 & y == 1)
)
expect_equal(
  .C_dropdims_which(x, y, .C_dropdims_count(x, y)),
  which(x == 1 & y == 1)
)

enumerate <- enumerate + 4L
