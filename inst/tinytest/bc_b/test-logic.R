
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_binary <- broadcast:::.test_binary
.test_binary_class <- broadcast:::.test_binary_class
.test_binary_zerolen <- broadcast:::.test_binary_zerolen
types <- c("logical", "integer", "raw")


logic_gates <- utils::read.csv("logic_gates.csv", sep = ";")


# logical ====
x <- logic_gates$A
y <- logic_gates$B


expect_equal(
  bc.b(x, y, "&") |> as.integer(),
  logic_gates$AND
)

expect_equal(
  bc.b(x, y, "|") |> as.integer(),
  logic_gates$OR
)

expect_equal(
  bc.b(x, y, "xor") |> as.integer(),
  logic_gates$XOR
)

expect_equal(
  bc.b(x, y, "nand") |> as.integer(),
  logic_gates$NAND
)

expect_equal(
  bc.b(x, y, "nor") |> as.integer(),
  logic_gates$NOR
)

enumerate <- enumerate + 5L


# raw ====
x <- as.raw(logic_gates$A)
y <- as.raw(logic_gates$B)


expect_equal(
  bc.b(x, y, "&") |> as.integer(),
  logic_gates$AND
)

expect_equal(
  bc.b(x, y, "|") |> as.integer(),
  logic_gates$OR
)

expect_equal(
  bc.b(x, y, "xor") |> as.integer(),
  logic_gates$XOR
)

expect_equal(
  bc.b(x, y, "nand") |> as.integer(),
  logic_gates$NAND
)

expect_equal(
  bc.b(x, y, "nor") |> as.integer(),
  logic_gates$NOR
)

enumerate <- enumerate + 5L

