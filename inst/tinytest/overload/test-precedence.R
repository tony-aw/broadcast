
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 1 or 5:
  out <- lapply(1:n, \(n)sample(c(1, 5), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}
.return_missing <- broadcast:::.return_missing

ab <- broadcast:::.as.broadcaster

mysub <- function(txt) {
  txt <- gsub("x", "ab(x)", txt)
  txt <- gsub("y", "ab(y)", txt)
  txt <- gsub("z", "ab(z)", txt)
  return(txt)
}

gen <- function() sample(c(rnorm(10), NA, NA, NaN, NaN, Inf, Inf, -Inf, -Inf))



# math ====
ops <- c("+", "-", "*", "/", "^")

for(iSample in 1:5) {
  x <- sample(c(rnorm(10), NA, NaN, -Inf, Inf), 100, TRUE)
  y <- sample(c(rnorm(10), NA, NaN, -Inf, Inf), 100, TRUE)
  z <- sample(c(rnorm(10), NA, NaN, -Inf, Inf), 100, TRUE)
  for(iOp1 in ops) {
    for(iOp2 in ops) {
      
      txt1 <- sprintf("x %s y %s z", iOp1, iOp2)
      txt2 <- sprintf("(x %s y) %s z", iOp1, iOp2)
      txt3 <- sprintf("x %s (y %s z)", iOp1, iOp2)
      
      out <- txt1 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt1))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      out <- txt2 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt2))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      out <- txt3 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt3))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
      
    }
  }
}


# Boolean ====
ops <- c("&", "|")

for(iSample in 1:5) {
  x <- sample(c(rnorm(10), NA, NaN, -Inf, Inf), 100, TRUE)
  y <- sample(c(rnorm(10), NA, NaN, -Inf, Inf), 100, TRUE)
  z <- sample(c(rnorm(10), NA, NaN, -Inf, Inf), 100, TRUE)
  for(iOp1 in ops) {
    for(iOp2 in ops) {
      
      txt1 <- sprintf("x %s y %s z", iOp1, iOp2)
      txt2 <- sprintf("(x %s y) %s z", iOp1, iOp2)
      txt3 <- sprintf("x %s (y %s z)", iOp1, iOp2)
      
      out <- txt1 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt1))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      out <- txt2 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt2))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      out <- txt3 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt3))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
      
    }
  }
}


# bit-wise ====
ops <- c("&", "|")

for(iSample in 1:5) {
  x <- sample(as.raw(0:255))
  y <- sample(as.raw(0:255))
  z <- sample(as.raw(0:255))
  for(iOp1 in ops) {
    for(iOp2 in ops) {
      
      txt1 <- sprintf("x %s y %s z", iOp1, iOp2)
      txt2 <- sprintf("(x %s y) %s z", iOp1, iOp2)
      txt3 <- sprintf("x %s (y %s z)", iOp1, iOp2)
      
      out <- txt1 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt1))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      out <- txt2 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt2))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      out <- txt3 |> mysub() |> str2expression() |> eval() |> unclass()
      expect <- eval(str2expression(txt3))
      expect_equivalent(
        out, expect
      ) |> errorfun()
      
      enumerate <- enumerate + 3L
      
    }
  }
}


