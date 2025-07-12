
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist

allocate_nested_list <- function(lens, depth) {
  out <- vector("list", lens[depth])
  if(depth == length(lens)) {
    return(out)
  }
  for(i in 1:lens[depth]) {
    out[[i]] <- allocate_nested_list(lens, depth + 1)
  }
  return(out)
}

basefun2 <- function(x, in2out = TRUE) {
  x.dim <- dim(x)
  if(in2out) {
    lens <- rev(x.dim)
    out <- .rcpp_allocate_nestedlist(lens, 1)
    
    for(i in 1:x.dim[1]) {
      for(j in 1:x.dim[2]) {
        temp <- x[[i, j]]
        if(is.null(temp)) {
          out[[j]][i] <- list(NULL)
        }
        else {
          out[[j]][[i]] <-  temp
        }
        
      }
    }
  }
  else {
    lens <- x.dim
    out <- .rcpp_allocate_nestedlist(lens, 1)
    
    for(i in 1:x.dim[1]) {
      for(j in 1:x.dim[2]) {
        temp <- x[[i, j]]
        if(is.null(temp)) {
          out[[i]][j] <- list(NULL)
        }
        else {
          out[[i]][[j]] <- temp
        }
        
      }
    }
  }
  return(out)
}


basefun3 <- function(x, in2out = TRUE) {
  x.dim <- dim(x)
  if(in2out) {
    lens <- rev(x.dim)
    out <- .rcpp_allocate_nestedlist(lens, 1)
    
    for(i in 1:x.dim[1]) {
      for(j in 1:x.dim[2]) {
        for(k in 1:x.dim[3]) {
          temp <- x[[i, j, k]]
          if(is.null(temp)) {
            out[[k]][[j]][i] <- list(NULL)
          }
          else {
            out[[k]][[j]][[i]] <-  temp
          }
        }
      }
    }
  }
  else {
    lens <- x.dim
    out <- .rcpp_allocate_nestedlist(lens, 1)
    
    for(i in 1:x.dim[1]) {
      for(j in 1:x.dim[2]) {
        for(k in 1:x.dim[3]) {
          temp <- x[[i, j, k]]
          if(is.null(temp)) {
            out[[i]][[j]][k] <- list(NULL)
          }
          else {
            out[[i]][[j]][[k]] <- temp
          }
        }
      }
    }
  }
  return(out)
}



# main function test ====

mat <- matrix(NA, 10, 10, dimnames = list(letters[1:10], LETTERS[1:10]))
attr(mat, "test") <- "test"

for(i in seq(2, 20, 2)) {
  nrowi <- i
  ncoli <- 20 / nrowi
  
  data <- sample(c(as.list(1:18), list(mat), list(NULL)), max(nrowi, ncoli))
  x <- matrix(
    data,
    nrowi, ncoli
  )
  expect_equal(
    basefun2(x),
    cast_dim2hier(x)
  ) |> errorfun()
  expect_equal(
    basefun2(x, FALSE),
    cast_dim2hier(x, FALSE)
  ) |> errorfun()
  
}


data <- sample(c(as.list(1:59), list(NULL)))
y <- array(data, sample(3:5))
expect_equal(
  basefun3(y),
  cast_dim2hier(y)
)
expect_equal(
  basefun3(y, FALSE),
  cast_dim2hier(y, FALSE)
)
enumerate <- enumerate + 2L



# internal tests ====

for(i in 1:10) {
  
  lens <- sample(1:10, sample(1:5, 1))
  
  out <- .rcpp_allocate_nestedlist(lens, 1)
  out2 <- allocate_nested_list(lens, 1)
  
  expect_equal(
    out,
    out2
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
  
}





# error tests ====
expect_error(
  cast_dim2hier(1:10),
  pattern = "`x` must be a list"
)
expect_error(
  cast_dim2hier(as.list(1:10)),
  pattern = "`x` has no dimensions"
)
x <- list()
dim(x) <- c(0, 10)
expect_error(
  cast_dim2hier(x),
  pattern = "`x` has zero-length dimensions"
)
x <- array(as.list(1:10), 10L)
expect_error(
  cast_dim2hier(x),
  pattern = "`x` is single-dimensional"
)
x <- array(as.list(1:10), rep(2, 17))
expect_error(
  cast_dim2hier(x),
  pattern = "arrays with more than 16 dimensions not supported"
)

x <- array(as.list(1:27), rep(3, 3))
expect_error(
  cast_dim2hier(x, NA),
  pattern = "`in2out` must be `TRUE` or `FALSE`"
)

enumerate <- enumerate + 7L


