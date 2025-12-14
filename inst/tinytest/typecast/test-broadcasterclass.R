# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# atomic ====

datal <- list(
  sample(as.raw(0:255)),
  sample(c(TRUE, FALSE, NA), 10L, TRUE),
  sample(c(-10:10, NA)),
  sample(c(rnorm(10), NA, NaN, Inf - Inf)),
  sample(c(rnorm(10), NA, NaN, Inf - Inf))  + sample(c(rnorm(10), NA, NaN, Inf - Inf)) * -1i,
  sample(c(month.abb, NA))
  # sample(list(letters, LETTERS, month.abb, month.name))
)

funs <- list(
  as_bool,
  as_int,
  as_dbl,
  as_cplx,
  as_chr,
  as_raw
)

for(iFun in seq_along(funs)) {
  for(iData in seq_along(datal)) {
    for(iBC in c(TRUE, FALSE)) {
      for(iMA in c(TRUE, FALSE)) {
        for(iCom in c(TRUE, FALSE)) {
          
          x <- datal[[iData]]
          fun <- funs[[iFun]]
          
          y <- x
          broadcaster(y) <- iBC
          expect_equal(
            broadcaster(fun(y)),
            iBC
          ) |> errorfun()
          
          if(iMA) {
            class(y) <- "mutatomic"
            broadcaster(y) <- iBC
          }
          
          if(iMA) {
            expect_true(
              inherits(fun(y), "mutatomic")
            ) |> errorfun()
          }
          else {
            expect_false(
              inherits(fun(y), "mutatomic")
            ) |> errorfun()
          }
          
          comment(y) <- "test comment"
          expect_equal(
            fun(y) |> comment(),
            comment(y)
          ) |> errorfun()
          
          enumerate <- enumerate + 3L
        }
        
      }
    }
  }
  
  
}


# list ====

datal <- list(
  sample(as.raw(0:255)),
  sample(c(TRUE, FALSE, NA), 10L, TRUE),
  sample(c(-10:10, NA)),
  sample(c(rnorm(10), NA, NaN, Inf - Inf)),
  sample(c(rnorm(10), NA, NaN, Inf - Inf))  + sample(c(rnorm(10), NA, NaN, Inf - Inf)) * -1i,
  sample(c(month.abb, NA)),
  sample(list(letters, LETTERS, month.abb, month.name))
)


for(iData in seq_along(datal)) {
  for(iBC in c(TRUE, FALSE)) {
    for(iMA in c(TRUE, FALSE)) {
      for(iCom in c(TRUE, FALSE)) {
        
        x <- datal[[iData]]
        fun <- as_list
        
        y <- x
        broadcaster(y) <- iBC
        expect_equal(
          broadcaster(fun(y)),
          iBC
        ) |> errorfun()
        
        if(iMA && is.atomic(y)) {
          class(y) <- "mutatomic"
          broadcaster(y) <- iBC
        }
        expect_false(
          inherits(fun(y), "mutatomic")
        ) |> errorfun()
        
        comment(y) <- "test comment"
        expect_equal(
          fun(y) |> comment(),
          comment(y)
        ) |> errorfun()
        
        enumerate <- enumerate + 3L
      }
      
    }
  }
}
