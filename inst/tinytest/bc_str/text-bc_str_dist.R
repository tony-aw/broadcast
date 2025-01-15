
enumerate <- 0L

expect_equal(
  bc.str(array("hello"), array("hello"), "levenshtein"),
  0L
)
expect_equal(
  bc.str(array("kitten"), array("sitting"), "levenshtein"),
  3L
)
expect_equal(
  bc.str(array("kitten"), array("kkkitten"), "levenshtein"),
  3L
)
expect_equal(
  bc.str(array("hello"), array("hellok"), "levenshtein"),
  1L
)
expect_equal(
  bc.str(array("helklo"), array("hello"), "levenshtein"),
  1L
)
enumerate <- enumerate + 5L