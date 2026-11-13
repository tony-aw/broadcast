
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.return_missing <- broadcast:::.return_missing

gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))



# arithmetic ====

accept_test <- function(x, y) {
  checkx <- broadcast:::.is_numeric_like(x) || is.complex(x) || is.raw(x)
  checky <- broadcast:::.is_numeric_like(y) || is.complex(y) || is.raw(y)
  return(!checkx || !checky)
}
pattern <- "non-numeric argument to binary operator"

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)
y.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)

for(iDataX in seq_along(x.data)) {
  for(iDataY in seq_along(y.data)) {
    x <- x.data[[iDataX]]
    y <- y.data[[iDataY]]
    
    
    if(accept_test(x, y)) {
      for(iBC in c(0, 1, 2)) {
        if(iBC == 0) {
          broadcaster(x) <- TRUE
        }
        else if(iBC == 1) {
          broadcaster(y) <- TRUE
        }
        else {
          broadcaster(x) <- TRUE
          broadcaster(y) <- TRUE
        }
        
        
        
        expect_error(
          x + y,
          pattern = pattern
        ) |> errorfun()
        
        expect_error(
          x - y,
          pattern = pattern
        )  |> errorfun()
        
        expect_error(
          x * y,
          pattern = pattern
        )  |> errorfun()
        
        expect_error(
          x / y,
          pattern = pattern
        )  |> errorfun()
        
        enumerate <- enumerate + 4L
        
        
      }
      
    }
    
  }
}


x <- as.raw(1)
y <- 1:10
broadcaster(x) <- TRUE
expect_error(
  x & y
)

enumerate <- enumerate + 2L



# invalid comparisons ====

accept_test <- function(x, y) {
  checkx <- is.logical(x) || is.numeric(x) || is.raw(x)
  checky <- is.logical(y) || is.numeric(y) || is.raw(y)
  return(!checkx || !checky)
}
pattern <- "invalid comparison with given types"

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)
y.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)

for(iDataX in seq_along(x.data)) {
  for(iDataY in seq_along(y.data)) {
    x <- x.data[[iDataX]]
    y <- y.data[[iDataY]]
    
    
    if(accept_test(x, y)) {
      for(iBC in c(0, 1, 2)) {
        if(iBC == 0) {
          broadcaster(x) <- TRUE
        }
        else if(iBC == 1) {
          broadcaster(y) <- TRUE
        }
        else {
          broadcaster(x) <- TRUE
          broadcaster(y) <- TRUE
        }
        
        
        
        expect_error(
          x < y,
          pattern = pattern
        ) |> errorfun()
        
        expect_error(
          x > y,
          pattern = pattern
        )  |> errorfun()
        
        expect_error(
          x <= y,
          pattern = pattern
        )  |> errorfun()
        
        expect_error(
          x >= y,
          pattern = pattern
        )  |> errorfun()
        
        enumerate <- enumerate + 4L
        
        
      }
      
    }
    
  }
}


x <- as.raw(1)
y <- 1:10
broadcaster(x) <- TRUE
expect_error(
  x & y
)

enumerate <- enumerate + 2L



# and/or ====

accept_test <- function(x, y) {
  checkx <- is.logical(x) || is.numeric(x) || is.complex(x)
  checky <- is.logical(y) || is.numeric(y) || is.complex(y)
  checkxy <- is.raw(x) && is.raw(y)
  return((!checkx || !checky) && !checkxy)
}
pattern <- "operations are possible only for numeric, logical or complex types"

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)
y.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)

for(iDataX in seq_along(x.data)) {
  for(iDataY in seq_along(y.data)) {
    x <- x.data[[iDataX]]
    y <- y.data[[iDataY]]
    
    
    if(accept_test(x, y)) {
      for(iBC in c(0, 1, 2)) {
        if(iBC == 0) {
          broadcaster(x) <- TRUE
        }
        else if(iBC == 1) {
          broadcaster(y) <- TRUE
        }
        else {
          broadcaster(x) <- TRUE
          broadcaster(y) <- TRUE
        }
        
        
        
        expect_error(
          x & y,
          pattern = pattern
        ) |> errorfun()
        
        expect_error(
          x | y,
          pattern = pattern
        )  |> errorfun()
        
        expect_error(
         xor(x, y),
          pattern = pattern
        )  |> errorfun()
        
        enumerate <- enumerate + 3L
        
        
      }
      
    }
    
  }
}


x <- as.raw(1)
y <- 1:10
broadcaster(x) <- TRUE
expect_error(
  x & y,
  pattern = "operations are possible only for numeric, logical or complex types"
)

enumerate <- enumerate + 1L



# integer ops ====

accept_test <- function(x, y) {
  checkx <- is.complex(x)
  checky <- is.complex(y)
  return(checkx || checky)
}
pattern <- "operator not supported for type `complex`"

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)
y.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i, # complex,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)

for(iDataX in seq_along(x.data)) {
  for(iDataY in seq_along(y.data)) {
    x <- x.data[[iDataX]]
    y <- y.data[[iDataY]]
    
    
    if(accept_test(x, y)) {
      for(iBC in c(0, 1, 2)) {
        if(iBC == 0) {
          broadcaster(x) <- TRUE
        }
        else if(iBC == 1) {
          broadcaster(y) <- TRUE
        }
        else {
          broadcaster(x) <- TRUE
          broadcaster(y) <- TRUE
        }
        
        
        
        expect_error(
          x %% y,
          pattern = pattern
        ) |> errorfun()
        
        expect_error(
          x %/% y,
          pattern = pattern
        )  |> errorfun()
        
        enumerate <- enumerate + 2L
        
        
      }
      
    }
    
  }
}



accept_test <- function(x, y) {
  checkx <- is.raw(x)
  checky <- is.raw(y)
  return(checkx || checky)
}
pattern <- "non-numeric argument to binary operator"

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)
y.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  sample(sample(letters, 100, TRUE)), # character
  sample(as.raw(0:255), 100) # raw
)

for(iDataX in seq_along(x.data)) {
  for(iDataY in seq_along(y.data)) {
    x <- x.data[[iDataX]]
    y <- y.data[[iDataY]]
    
    
    if(accept_test(x, y)) {
      for(iBC in c(0, 1, 2)) {
        if(iBC == 0) {
          broadcaster(x) <- TRUE
        }
        else if(iBC == 1) {
          broadcaster(y) <- TRUE
        }
        else {
          broadcaster(x) <- TRUE
          broadcaster(y) <- TRUE
        }
        
        
        
        expect_error(
          x %% y,
          pattern = pattern
        ) |> errorfun()
        
        expect_error(
          x %/% y,
          pattern = pattern
        )  |> errorfun()
        
        enumerate <- enumerate + 2L
        
        
      }
      
    }
    
  }
}

# power with complex numbers ====

accept_test <- function(x, y) {
  checkx <- is.complex(x)
  checky <- is.complex(y)
  return(checkx || checky)
}
pattern <- "operator not defined for complex numbers"

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i # complex
)
y.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE), # logical
  sample(c(-10:10, NA), 100, TRUE), # integer
  gen(), # double,
  gen() + gen() * -1i # complex
)

for(iDataX in seq_along(x.data)) {
  for(iDataY in seq_along(y.data)) {
    x <- x.data[[iDataX]]
    y <- y.data[[iDataY]]
    
    
    if(accept_test(x, y)) {
      for(iBC in c(0, 1, 2)) {
        if(iBC == 0) {
          broadcaster(x) <- TRUE
        }
        else if(iBC == 1) {
          broadcaster(y) <- TRUE
        }
        else {
          broadcaster(x) <- TRUE
          broadcaster(y) <- TRUE
        }
        
        
        
        expect_error(
          x ^ y,
          pattern = pattern
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
        
      }
      
    }
    
  }
}


