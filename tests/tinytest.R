
if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package(
    "broadcast", testdir = "tinytest"
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "internal")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_num")
  )
}

