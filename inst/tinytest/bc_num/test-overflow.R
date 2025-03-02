expect_equal(
  bc.i(2^54, 1, "+") |> drop(),
  Inf
)

expect_equal(
  bc.i(-2^54, 1, "-") |> drop(),
  -Inf
)
