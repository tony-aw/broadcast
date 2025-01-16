
enumerate <- 0L

expect_equal(
  bc.str(array("hello"), array("hello"), "levenshtein"),
  array(0L)
)
expect_equal(
  bc.str(array("kitten"), array("sitting"), "levenshtein"),
  array(3L)
)
expect_equal(
  bc.str(array("kitten"), array("kkkitten"), "levenshtein"),
  array(2L)
)
expect_equal(
  bc.str(array("hello"), array("hellok"), "levenshtein"),
  array(1L)
)
expect_equal(
  bc.str(array("helklo"), array("hello"), "levenshtein"),
  array(1L)
)
enumerate <- enumerate + 5L
