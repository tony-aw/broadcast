# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

source(file.path(getwd(), "source.R"))

# too many dimensions error ====
message <- "arrays with more than 16 dimensions are not supported"
for(i in seq_along(funs)) {
  x <- array(datagens[[i]](), dim = rep(2L, 17L))
  expect_error(
    funs[[i]](x, as.vector(x), ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](as.vector(x), x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](x, x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}



# op must be a single string ====
message <- "`op` must be single string"
op <- rep("==", 2L)
for(i in 1:7) {
  x <- datagens[[i]]()
  expect_error(
    funs[[i]](x, x, op),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}


# non-conformable vectors ====
message <- "`x` and `y` are not conformable"
for(i in seq_along(funs)) {
  
  x <- datagens[[i]]()
  y <- x[1:2]
  
  expect_error(
    funs[[i]](x, y, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  expect_error(
    funs[[i]](y, x, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
}


# non-conformable arrays ====
message <- "`x` and `y` are not conformable"
for(i in seq_along(funs)) {
  
  x <- array(datagens[[i]](), c(2, 10))
  y <- array(datagens[[i]](), c(10, 2))
  
  expect_error(
    funs[[i]](x, y, ops[[i]]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}


# broadcasting will exceed maximum size ====
pattern <- "broadcasting will exceed maximum size"
maxint <- 2^53 + 1L
n <- ceiling(sqrt(maxint))
x <- array(as.raw(0:255), c(n, 1))
y <- array(as.raw(0:255), c(1, n))
expect_error(
  bc.raw(x, y, "diff"),
  pattern = pattern
)
broadcaster(x) <- broadcaster(y) <- TRUE
expect_error(
  x & y,
  pattern = pattern
)

x <- 1:(2^32 + 2)
y <- array(1:27, c(1, 3, 3))
expect_error(
  bc.d(x, y, "+"),
  pattern = pattern
)
broadcaster(x) <- broadcaster(y) <- TRUE
expect_error(
  x + y,
  pattern = pattern
)
enumerate <- enumerate + 4L

 
# operator errors ====
message <- "given operator not supported in the given context"
ops <- c(
  "+", "&", "&", "<", "<", "+", "*"
)
for(i in 1:7) {
  x <- datagens[[i]]()
  expect_error(
    funs[[i]](x, x, ops[i]),
    pattern = message
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
}


# type errors - numeric ====
pattern <- "`x` and `y` must be "
good_type <- broadcast:::.is_numeric_like

for(typeX in seq_along(datagens)) {
  for(typeY in seq_along(datagens)) {
    
    x <- array(datagens[[typeX]]())
    y <- array(datagens[[typeY]]())
    
    if(!good_type(x) || !good_type(y)) {
      expect_error(
        bc.i(x, y, "+"),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      expect_error(
        bc.i(x, y, "=="),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      expect_error(
        bc.d(x, y, "+"),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      expect_error(
        bc.d(x, y, "=="),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      enumerate <- enumerate + 4L
      
    }
  }
}

# type errors - Boolean ====
pattern <- "unsupported types given"
good_type <- \(x) broadcast:::.is_boolable(x) || is.numeric(x)

for(typeX in seq_along(datagens)) {
  for(typeY in seq_along(datagens)) {
    
    x <- array(datagens[[typeX]]())
    y <- array(datagens[[typeY]]())
    
    if(!good_type(x) || !good_type(y)) {
      expect_error(
        bc.b(x, y, "&"),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      expect_error(
        bc.b(x, y, "=="),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
      
    }
  }
}


# type errors - complex ====
pattern <- "`x` and `y` must be "
good_type <- is.complex

for(typeX in seq_along(datagens)) {
  for(typeY in seq_along(datagens)) {
    
    x <- array(datagens[[typeX]]())
    y <- array(datagens[[typeY]]())
    
    if(!good_type(x) || !good_type(y)) {
      expect_error(
        bc.cplx(x, y, "+"),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      expect_error(
        bc.cplx(x, y, "=="),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
      
    }
  }
}



# type errors - character ====
pattern <- "`x` and `y` must be "
good_type <- is.character

for(typeX in seq_along(datagens)) {
  for(typeY in seq_along(datagens)) {
    
    x <- array(datagens[[typeX]]())
    y <- array(datagens[[typeY]]())
    
    if(!good_type(x) || !good_type(y)) {
      expect_error(
        bc.str(x, y, "+"),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      expect_error(
        bc.str(x, y, "=="),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      expect_error(
        bc.str(x, y, "levenshtein"),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
      
    }
  }
}


# type errors - raw ====
pattern <- "`x` and `y` must be "
good_type <- is.raw
for(typeX in seq_along(datagens)) {
  for(typeY in seq_along(datagens)) {
    
    x <- array(datagens[[typeX]]())
    y <- array(datagens[[typeY]]())
    
    if(!good_type(x) || !good_type(y)) {
      expect_error(
        bc.raw(x, y, "=="),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}




# type errors - bits ====
pattern <- "`x` and `y` must both be "
good_type <- \(x) is.raw(x) || is.integer(x)

for(typeX in seq_along(datagens)) {
    
  x <- array(datagens[[typeX]]())
  y <- array(datagens[[typeX]]())
  
  if(is.double(x)) {
    expect_error(
      bc.bit(x, y, "&"),
      pattern = "only 32-bit integers allowed, not 53-bit integers",
      fixed = TRUE
    ) |> errorfun()
    expect_error(
      bc.bit(x, y, "=="),
      pattern = "only 32-bit integers allowed, not 53-bit integers",
      fixed = TRUE
    ) |> errorfun()
    enumerate <- enumerate + 2L
  }
  
  if(!good_type(x) && !is.double(x)) {
    expect_error(
      bc.bit(x, y, "&"),
      pattern = pattern,
      fixed = TRUE
    ) |> errorfun()
    expect_error(
      bc.bit(x, y, "=="),
      pattern = pattern,
      fixed = TRUE
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
    
  }
}



# type errors - list ====
pattern <- "`x` and `y` must be "
good_type <- is.list

for(typeX in seq_along(datagens)) {
  for(typeY in seq_along(datagens)) {
    
    x <- array(datagens[[typeX]]())
    y <- array(datagens[[typeY]]())
    
    if(!good_type(x) || !good_type(y)) {
      expect_error(
        bc.list(x, y, \(x, y)paste0(x, y)),
        pattern = pattern,
        fixed = TRUE
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}


# type errors - general relational operators ====
expect_error(
  bc.rel(as.list(1:10), as.list(1:10), "=="),
  pattern = "only atomic arrays supported for general relational operators"
)
expect_error(
  bc.rel(1:10, 1:10, "+"),
  pattern = "given operator not supported in the given context"
)
enumerate <- enumerate + 1L

