
if ( requireNamespace("tinytest", quietly = TRUE) ){
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
    "broadcast", testdir = file.path("tinytest", "bc_bytes")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_list")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bc_general")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_1_basic")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_2_pre")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_3_row")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_4_col")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_5_lyr")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "bind_array_6_post")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "othercast")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "overload")
  )
  tinytest::test_package(
    "broadcast", testdir = file.path("tinytest", "class")
  )
}

