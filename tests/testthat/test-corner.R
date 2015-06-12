context("corner")

test_that("empty web", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  web <- rpkgweb(tmp_dir)
  expect_equal(length(web), 0)
  expect_match(format(web), "without any packages")

  write_makefile(web)

  devtools::in_dir(tmp_dir, {
    res <- system2("make", "-n", stdout = TRUE, stderr = TRUE)
    expect_null(attr(res, "status"))
  })
})
