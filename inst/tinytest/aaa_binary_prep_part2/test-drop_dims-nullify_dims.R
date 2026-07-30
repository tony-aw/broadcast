

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_virt_drop_dims <- broadcast:::.rcpp_virt_drop_dims
.rcpp_clone <- broadcast:::.rcpp_clone


# x and y have length 1 ====
for(iNdim in 0:4) {
  x.dim <- rep(1L, 4L) |> .rcpp_clone()
  y.dim <- rep(1L, 4L) |> .rcpp_clone()
  x.ndim <- .rcpp_clone(iNdim)
  y.ndim <- .rcpp_clone(iNdim)
  x.len <- 1L
  y.len <- 1L
  
  .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
  
  expect_equal(
    c(x.ndim, y.ndim),
    c(0L, 0L)
  ) |> errorfun()
  expect_equal(
    x.dim,
    rep(1L, 4L)
  ) |> errorfun()
  expect_equal(
    y.dim,
    rep(1L, 4L)
  ) |> errorfun()
  
  
  enumerate <- enumerate + 3L
  
}

for(iNdim in 5:16) {
  x.dim <- rep(1L, 16L) |> .rcpp_clone()
  y.dim <- rep(1L, 16L) |> .rcpp_clone()
  x.ndim <- .rcpp_clone(iNdim)
  y.ndim <- .rcpp_clone(iNdim)
  x.len <- 1L
  y.len <- 1L
  
  .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
  
  expect_equal(
    c(x.ndim, y.ndim),
    c(0L, 0L)
  ) |> errorfun()
  expect_equal(
    x.dim,
    rep(1L, 16L)
  ) |> errorfun()
  expect_equal(
    y.dim,
    rep(1L, 16L)
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
  
}


# x has length 1, y has length > 1 ====
for(iSample in 1:10) {
  for(iNdim in 0:4) {
    for(iLen in 2:10) {
      x.dim <- rep(1L, 4L) |> .rcpp_clone()
      y.dim <- rep(1L, 4L) |> .rcpp_clone()
      
      x.ndim <- .rcpp_clone(iNdim)
      y.ndim <- .rcpp_clone(max(1L, iNdim))
      x.len <- 1L
      y.len <- iLen
      
      y.dim[sample(1:y.ndim, 1L)] <- iLen
      
      .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
      
      expect_equal(
        c(x.ndim, y.ndim),
        c(0L, 0L)
      ) |> errorfun()
      expect_equal(
        x.dim,
        rep(1L, 4L)
      ) |> errorfun()
      expect_equal(
        y.dim,
        rep(1L, 4L)
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
    }
  }
}

for(iSample in 1:10) {
  for(iNdim in 5:16) {
    for(iLen in 2:10) {
      x.dim <- rep(1L, 16L) |> .rcpp_clone()
      y.dim <- rep(1L, 16L) |> .rcpp_clone()
      
      x.ndim <- .rcpp_clone(iNdim)
      y.ndim <- .rcpp_clone(max(1L, iNdim))
      x.len <- 1L
      y.len <- iLen
      
      y.dim[sample(1:y.ndim, 1L)] <- iLen
      
      .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
      
      expect_equal(
        c(x.ndim, y.ndim),
        c(0L, 0L)
      ) |> errorfun()
      expect_equal(
        x.dim,
        rep(1L, 16L)
      ) |> errorfun()
      expect_equal(
        y.dim,
        rep(1L, 16L)
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
    }
  }
}



# x has length > 1, y has length 1 ====
for(iSample in 1:10) {
  for(iNdim in 0:4) {
    for(iLen in 2:10) {
      x.dim <- rep(1L, 4L) |> .rcpp_clone()
      y.dim <- rep(1L, 4L) |> .rcpp_clone()
      
      x.ndim <- .rcpp_clone(max(1L, iNdim))
      y.ndim <- .rcpp_clone(iNdim)
      x.len <- iLen
      y.len <- 1L
      
      x.dim[sample(1:x.ndim, 1L)] <- iLen
      
      .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
      
      expect_equal(
        c(x.ndim, y.ndim),
        c(0L, 0L)
      ) |> errorfun()
      expect_equal(
        x.dim,
        rep(1L, 4L)
      ) |> errorfun()
      expect_equal(
        y.dim,
        rep(1L, 4L)
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
    }
  }
}

for(iSample in 1:10) {
  for(iNdim in 5:16) {
    for(iLen in 2:10) {
      x.dim <- rep(1L, 16L) |> .rcpp_clone()
      y.dim <- rep(1L, 16L) |> .rcpp_clone()
      
      x.ndim <- .rcpp_clone(max(1L, iNdim))
      y.ndim <- .rcpp_clone(iNdim)
      x.len <- iLen
      y.len <- 1L
      
      x.dim[sample(1:x.ndim, 1L)] <- iLen
      
      .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
      
      expect_equal(
        c(x.ndim, y.ndim),
        c(0L, 0L)
      ) |> errorfun()
      expect_equal(
        x.dim,
        rep(1L, 16L)
      ) |> errorfun()
      expect_equal(
        y.dim,
        rep(1L, 16L)
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
    }
  }
}



# x is vector, y is 1d ====
for(iLen in 1:10) {
  for(iNchunk in c(4L, 16L)) {
    x.dim <- rep(1L, iNchunk)
    y.dim <- c(iLen, rep(1L, iNchunk - 1L))
    x.ndim <- 0L
    y.ndim <- 1L
    
    x.len <- y.len <- iLen
    
    .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
    
    expect_equal(
      c(x.ndim, y.ndim),
      c(0L, 0L)
    ) |> errorfun()
    expect_equal(
      x.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    expect_equal(
      y.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    
    enumerate <- enumerate + 3L
    
  }
}


# x is 1d, y is vector ====
for(iLen in 1:10) {
  for(iNchunk in c(4L, 16L)) {
    x.dim <- c(iLen, rep(1L, iNchunk - 1L))
    y.dim <- rep(1L, iNchunk)
    x.ndim <- 1L
    y.ndim <- 0L
    
    x.len <- y.len <- iLen
    
    .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
    
    expect_equal(
      c(x.ndim, y.ndim),
      c(0L, 0L)
    ) |> errorfun()
    expect_equal(
      x.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    expect_equal(
      y.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    
    enumerate <- enumerate + 3L
    
  }
}



# x and y are vectors ====
for(iLen in 1:10) {
  for(iNchunk in c(4L, 16L)) {
  
    x.dim <- rep(1L, iNchunk)
    y.dim <- rep(1L, iNchunk)
    x.ndim <- 0L
    y.ndim <- 0L
    
    x.len <- y.len <- iLen
    
    .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
    
    expect_equal(
      c(x.ndim, y.ndim),
      c(0L, 0L)
    ) |> errorfun()
    expect_equal(
      x.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    expect_equal(
      y.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    
    enumerate <- enumerate + 3L
  }
}

# x and y are 1d ====
for(iLen in 1:10) {
  for(iNchunk in c(4L, 16L)) {
  
    x.dim <- c(iLen, rep(1L, iNchunk - 1L))
    y.dim <- c(iLen, rep(1L, iNchunk - 1L))
    x.ndim <- 1L
    y.ndim <- 1L
    
    x.len <- y.len <- iLen
    
    .rcpp_virt_drop_dims(x.dim, y.dim, x.ndim, y.ndim, x.len, y.len)
    
    expect_equal(
      c(x.ndim, y.ndim),
      c(0L, 0L)
    ) |> errorfun()
    expect_equal(
      x.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    expect_equal(
      y.dim,
      rep(1L, iNchunk)
    ) |> errorfun()
    
    enumerate <- enumerate + 3L
  }
  
}



