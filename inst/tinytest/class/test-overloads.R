
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


ops <- c(
  "+", "-", "*", "/", "^", "%%", "%/%",
  "==", "!=", "<", ">", "<=", ">=",
  "&", "|"
)

x <- 0:9
y <- array(0:9, c(1, 10))

for(xB in c(TRUE, FALSE)) {
  for(yB in c(TRUE, FALSE)) {
    for(op in ops) {
      
      broadcaster(x) <- xB
      broadcaster(y) <- yB
      
      if(broadcaster(x) || broadcaster(y)) {
        
        
        txt <- paste0("x", op, "y")
        expected <- bc_chain(as.formula(paste0("~ ", txt)))
        out <- eval(str2expression(txt))
        
        expect_equal(
          expected,
          out
        ) |> errorfun()
        enumerate <- enumerate + 1L
        
      }
      
    }
  }
}

