context("rpkgweb")

test_that("dirname = pkgname", {
  expect_error(rpkgweb("test_web_dirname"), "directory names .* package names")
  expect_error(rpkgweb("test_web_dirname"), "AA != A")
  expect_error(rpkgweb("test_web_dirname"), "BBB != B")
  expect_error(rpkgweb("test_web_dirname"), "DD != D")
})
