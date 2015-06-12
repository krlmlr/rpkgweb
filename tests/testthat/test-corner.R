context("corner")

test_that("empty web", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  web <- rpkgweb(tmp_dir)
  expect_equal(length(web), 0)
  expect_match(format(web), "without any packages")
})
