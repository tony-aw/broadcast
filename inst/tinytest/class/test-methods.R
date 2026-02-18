
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# sub-setting, interior indices ===

x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
for(i in 1:20) {
  expect_equal(
    broadcast:::.as.broadcaster(x[i]),
    y[i]
  ) |> errorfun()
  ind <- sample(1:20, i)
  expect_equal(
    broadcast:::.as.broadcaster(x[ind]),
    y[ind]
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}


# sub-setting, drop = FALSE ====

x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
for(i in 1:5) {
  for(j in 1:4) {
    expect_equal(
      broadcast:::.as.broadcaster(x[i, j,drop = FALSE]),
      y[i, j, drop = FALSE]
    ) |> errorfun()
    
    ind1 <- sample(1:5, i)
    ind2 <- sample(1:4, j)
    expect_equal(
      broadcast:::.as.broadcaster(x[ind1, ind2,drop = FALSE]),
      y[ind1, ind2, drop = FALSE]
    ) |> errorfun()
    
    expect_equal(
      broadcast:::.as.broadcaster(x[, ind2,drop = FALSE]),
      y[, ind2, drop = FALSE]
    ) |> errorfun()
    
    expect_equal(
      broadcast:::.as.broadcaster(x[ind1, drop = FALSE]),
      y[ind1,  drop = FALSE]
    ) |> errorfun()
    
    enumerate <- enumerate + 5L
    
  }
}



# sub-setting, drop = TRUE ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
for(i in 1:5) {
  for(j in 1:4) {
    expect_equal(
      broadcast:::.as.broadcaster(x[i, j,drop = TRUE]),
      y[i, j, drop = TRUE]
    ) |> errorfun()
    
    ind1 <- sample(1:5, i)
    ind2 <- sample(1:4, j)
    expect_equal(
      broadcast:::.as.broadcaster(x[ind1, ind2,drop = TRUE]),
      y[ind1, ind2, drop = TRUE]
    ) |> errorfun()
    
    expect_equal(
      broadcast:::.as.broadcaster(x[, ind2,drop = TRUE]),
      y[, ind2, drop = TRUE]
    ) |> errorfun()
    
    expect_equal(
      broadcast:::.as.broadcaster(x[ind1, drop = TRUE]),
      y[ind1,  drop = TRUE]
    ) |> errorfun()
    
    enumerate <- enumerate + 4L
    
  }
}


# sub-setting2 ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)

for(i in 1:20) {
  expect_equal(
    broadcast:::.as.broadcaster(x[[i]]),
    y[[i]]
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


for(i in 1:5) {
  for(j in 1:4) {
    expect_equal(
      broadcast:::.as.broadcaster(x[[i, j]]),
      y[[i, j]]
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}


enumerate <- enumerate + 2L




#  replacement ====
for(i in 1:20) {
  x <- matrix(1:20, ncol = 4)
  y <- broadcast:::.as.broadcaster(x)
  x[i] <- -1
  y[i] <- -1
  expect_equal(
    broadcast:::.as.broadcaster(x),
    y
  ) |> errorfun()
  
  ind <- sample(1:20, i)
  x <- matrix(1:20, ncol = 4)
  y <- broadcast:::.as.broadcaster(x)
  x[ind] <- -1
  y[ind] <- -1
  expect_equal(
    broadcast:::.as.broadcaster(x),
    y
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}

for(i in 1:5) {
  for(j in 1:4) {
    x <- matrix(1:20, ncol = 4)
    y <- broadcast:::.as.broadcaster(x)
    x[i, j] <- -1
    y[i, j] <- -1
    expect_equal(
      broadcast:::.as.broadcaster(x),
      y
    ) |> errorfun()
    
    ind1 <- sample(1:5, i)
    ind2 <- sample(1:4, j)
    x <- matrix(1:20, ncol = 4)
    y <- broadcast:::.as.broadcaster(x)
    x[ind1, ind2] <- -1
    y[ind1, ind2] <- -1
    expect_equal(
      broadcast:::.as.broadcaster(x),
      y
    ) |> errorfun()
    
    x <- matrix(1:20, ncol = 4)
    y <- broadcast:::.as.broadcaster(x)
    x[, ind2] <- -1
    y[, ind2] <- -1
    expect_equal(
      broadcast:::.as.broadcaster(x),
      y
    ) |> errorfun()
    
    x <- matrix(1:20, ncol = 4)
    y <- broadcast:::.as.broadcaster(x)
    x[ind1, ] <- -1
    y[ind1, ] <- -1
    expect_equal(
      broadcast:::.as.broadcaster(x),
      y
    ) |> errorfun()
    
    enumerate <- enumerate + 4L
  }
}




#  replacement2 ====
for(i in 1:20) {
  x <- matrix(1:20, ncol = 4)
  y <- broadcast:::.as.broadcaster(x)
  x[[i]] <- -1
  y[[i]] <- -1
  expect_equal(
    broadcast:::.as.broadcaster(x),
    y
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

for(i in 1:5) {
  for(j in 1:4) {
    x <- matrix(1:20, ncol = 4)
    y <- broadcast:::.as.broadcaster(x)
    x[[i, j]] <- -1
    y[[i, j]] <- -1
    expect_equal(
      broadcast:::.as.broadcaster(x),
      y
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
}


# errors ====
x <- 1:10
expect_error(
  broadcast:::`[.broadcaster`(x, 1),
  pattern = "malformed broadcaster"
)
expect_error(
  broadcast:::`[[.broadcaster`(x, 1),
  pattern = "malformed broadcaster"
)

