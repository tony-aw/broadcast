
if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package(
    "broadcast", testdir = "tinytest"
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "internal")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_d")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_i")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_b")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_str")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_cplx")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_list")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_generic")
  )
}

