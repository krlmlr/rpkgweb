context("bump")

test_that("formatting", {
  expect_equal(get_sprintf_format("0.0", 2), "%s.%s")
  expect_equal(get_sprintf_format("0.0-0", 2), "%s.%s")
  expect_equal(get_sprintf_format("0.0-0", 3), "%s.%s-%s")
  expect_equal(get_sprintf_format("0.0-0", 5), "%s.%s-%s.%s.%s")
  expect_equal(get_sprintf_format("0.0", 1), "%s.%s")
})

test_that("increasing", {
  expect_equal(increase_version("0.0", 2, "0.0"), "0.1")
  expect_equal(increase_version("0.0-0", 2, "0.0-0"), "0.1")
  expect_equal(increase_version("0.0-10", 3, "0.0-0"), "0.0-11")
  expect_equal(increase_version("0.0-10", 5, "0.0-0"), "0.0-10.0.1")
  expect_equal(increase_version("0.0", 1, "0.0"), "1.0")
})
