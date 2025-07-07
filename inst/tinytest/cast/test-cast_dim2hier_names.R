
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# fully named, in2out = TRUE ====
x <- array(as.list(1:60), 5:3)
dimnames(x) <- list(
  letters[1:5],
  LETTERS[1:4],
  month.abb[1:3]
)

out <- cast_dim2hier(x, distr.names = TRUE)
expect_equal(
  names(out),
  dimnames(x)[[3]]
)

for(i in seq_along(out)) {
  temp <- out[[i]]
  expect_equal(
    names(temp),
    dimnames(x)[[2]]
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
  for(j in seq_along(temp)) {
    temp2 <- temp[[j]]
    expect_equal(
      names(temp2),
      dimnames(x)[[1]]
    ) |> errorfun()
    enumerate <- enumerate + 1L
    
  }
}



# fully named, in2out = FALSE ====
x <- array(as.list(1:60), 5:3)
dimnames(x) <- list(
  letters[1:5],
  LETTERS[1:4],
  month.abb[1:3]
)

out <- cast_dim2hier(x, in2out = FALSE, distr.names = TRUE)
expect_equal(
  names(out),
  dimnames(x)[[1]]
)

for(i in seq_along(out)) {
  temp <- out[[i]]
  expect_equal(
    names(temp),
    dimnames(x)[[2]]
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
  for(j in seq_along(temp)) {
    temp2 <- temp[[j]]
    expect_equal(
      names(temp2),
      dimnames(x)[[3]]
    ) |> errorfun()
    enumerate <- enumerate + 1L
    
  }
}


# partially named, in2out = TRUE ====
for(d in 1:3) {
  x <- array(as.list(1:60), 5:3)
  x.dimnames <- list(
    letters[1:5],
    LETTERS[1:4],
    month.abb[1:3]
  )
  dimnames(x) <- x.dimnames
  
  out <- cast_dim2hier(x, distr.names = TRUE)
  expect_equal(
    names(out),
    dimnames(x)[[3]]
  )
  
  for(i in seq_along(out)) {
    temp <- out[[i]]
    expect_equal(
      names(temp),
      dimnames(x)[[2]]
    ) |> errorfun()
    enumerate <- enumerate + 1L
    
    for(j in seq_along(temp)) {
      temp2 <- temp[[j]]
      expect_equal(
        names(temp2),
        dimnames(x)[[1]]
      ) |> errorfun()
      enumerate <- enumerate + 1L
      
    }
  }
}


# partially named, in2out = FALSE ====
for(d in 1:3) {
  x <- array(as.list(1:60), 5:3)
  x.dimnames <- list(
    letters[1:5],
    LETTERS[1:4],
    month.abb[1:3]
  )
  dimnames(x) <- x.dimnames
  
  out <- cast_dim2hier(x, in2out = FALSE, distr.names = TRUE)
  expect_equal(
    names(out),
    dimnames(x)[[1]]
  )
  
  for(i in seq_along(out)) {
    temp <- out[[i]]
    expect_equal(
      names(temp),
      dimnames(x)[[2]]
    ) |> errorfun()
    enumerate <- enumerate + 1L
    
    for(j in seq_along(temp)) {
      temp2 <- temp[[j]]
      expect_equal(
        names(temp2),
        dimnames(x)[[3]]
      ) |> errorfun()
      enumerate <- enumerate + 1L
      
    }
  }
}

# error ====
expect_error(
  cast_dim2hier(x, distr.names = NA),
  pattern = "`distr.names` must be `TRUE` or `FALSE`"
)
enumerate <- enumerate + 1L


