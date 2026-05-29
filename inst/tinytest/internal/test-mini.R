
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


# dim is vector ====

.C_dims_is_vector <- broadcast:::.C_dims_is_vector

for(i in 1:10) {
  expect_true(.C_dims_is_vector(1L)) |> errorfun()
  enumerate <- enumerate + 1L
}

for(i in 1:16) {
  for(iSample in 1:10) {
    x.dim <- sample(c(1L, 0L), i, TRUE)
    expect_true(.C_dims_is_vector(x.dim)) |> errorfun()
    enumerate <- enumerate + 1L
  }
}

for(iSample in 1:10) {
  for(i in 2:16) {
    x.dim <- sample(c(10L, rep_len(1L, i - 1L)))
    expect_true(.C_dims_is_vector(x.dim)) |> errorfun()
  }
  enumerate <- enumerate + 1L
}

for(iSample in 1:10) {
  for(i in 2:16) {
    x.dim <- sample(c(10L, 10L, rep_len(1L, i - 2L)))
    expect_false(.C_dims_is_vector(x.dim)) |> errorfun()
  }
  enumerate <- enumerate + 1L
}


for(iSample in 1:10) {
  for(i in 3:16) {
    x.dim <- sample(1:i)
    expect_false(.C_dims_is_vector(x.dim)) |> errorfun()
  }
  enumerate <- enumerate + 1L
}




