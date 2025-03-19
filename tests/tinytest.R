
if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package(
    "broadcast", testdir = "tinytest"
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "internal")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_b")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_num")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_cplx")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_str")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_list")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_generic")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_pre")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_row")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_col")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_lyr")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_post")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_other")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "othercast")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "misc")
  )
}

