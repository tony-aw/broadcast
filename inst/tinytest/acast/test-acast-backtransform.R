
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

source("source.R")

for(i in seq_along(datagens)) {
  margin <- 1L
  x.dim <- c(5, 5, 5)
  x.dim[margin] <- 10L
  x <- array(datagens[[i]](), x.dim)
  dimnames(x) <- lapply(dim(x), \(n)sample(letters, n))
  grp <- factor(rep(letters[1:5], 2))
  
  out <- acast(x, margin, grp)
  back <- asplit(out, ndim(out)) |> bind_array(along = margin)
  expect_equivalent(
    back,
    x[order(grp), , ]
  ) |> errorfun()
  
  margin <- 2L
  x.dim <- c(5, 5, 5)
  x.dim[margin] <- 10L
  x <- array(datagens[[i]](), x.dim)
  dimnames(x) <- lapply(dim(x), \(n)sample(letters, n))
  grp <- factor(rep(letters[1:5], 2))
  
  out <- acast(x, margin, grp)
  back <- asplit(out, ndim(out)) |> bind_array(along = margin)
  expect_equivalent(
    back,
    x[, order(grp) , ]
  ) |> errorfun()
  
  margin <- 3L
  x.dim <- c(5, 5, 5)
  x.dim[margin] <- 10L
  x <- array(datagens[[i]](), x.dim)
  dimnames(x) <- lapply(dim(x), \(n)sample(letters, n))
  grp <- factor(rep(letters[1:5], 2))
  
  out <- acast(x, margin, grp)
  back <- asplit(out, ndim(out)) |> bind_array(along = margin)
  expect_equivalent(
    back,
    x[, , order(grp)]
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}

