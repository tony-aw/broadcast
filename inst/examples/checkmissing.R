
# checkNA ====
x <- array(
  sample(c(-10:10, NA, NaN)), dim = 4:2
)
y <- array(
  sample(c(-10:10, NA, NaN)), dim = c(4,1,1)
)
broadcaster(x) <- broadcaster(y) <- TRUE

mx <- checkNA(x, "raw")
my <- checkNA(y, "raw")
bc.b(mx, my, "&")
bc.b(mx, my, "xor")
bc.b(mx, my, "nand")
bc.b(mx, my, "==")
bc.b(mx, my, "!=")
bc_ifelse(bc.b(mx, my, "|"), -1000L, x + y)


# checkNULL ====
x <- array(
  sample(list(letters, LETTERS, month.abb, month.name, NULL)), dim = 4:2
)
y <- array(
  sample(list(letters, LETTERS, month.abb, month.name, NULL)), dim = c(4,1,1)
)
broadcaster(x) <- broadcaster(y) <- TRUE

mx <- checkNULL(x, "raw")
my <- checkNULL(y, "raw")
bc.b(mx, my, "&")
bc.b(mx, my, "xor")
bc.b(mx, my, "nand")
bc.b(mx, my, "==")
bc.b(mx, my, "!=")
bc_ifelse(bc.b(mx, my, "|"), list(~ "Nothing"), bc.list(x, y, paste0))

