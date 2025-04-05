
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

tempfun <- function(x, margin, grp, fill_val) {
  maxfreq <- max(tabulate(unclass(grp)))
  out.dim <- c(dim(x), length(unique(grp)))
  out.dim[1L] <- maxfreq
  out <- array(fill_val, out.dim)
  
  for(k in 1:nlevels(grp)) {
    extract <- x[grp == levels(grp)[k],,  drop=FALSE]
    out[1:nrow(extract), 1:ncol(extract), k] <- extract
  }
  
  return(out)
}

x <- array(1:16, c(4,4))
grp <- as.factor(rep_len(1:3, 4))


# atomic types ====
datagens <- list(
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA))
)

NAs <- list(
  NA,
  NA_integer_,
  NA_real_,
  NA_complex_,
  NA_character_
)

for(i in seq_along(datagens)) {
  for(j in NAs) {
    x <- array(datagens[[i]](), c(4,4))
    expect_equivalent(
      acast(x, 1, grp, TRUE, fill_val = j),
      tempfun(x, 1, grp, NA)
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}


# recursive ====

for(i in seq_along(datagens)) {
  for(j in NAs) {
    x <- array(as.list(datagens[[i]]()), c(4,4))
    expect_equivalent(
      acast(x, 1, grp, TRUE, fill_val = list(j)),
      tempfun(x, 1, grp, list(j))
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}

