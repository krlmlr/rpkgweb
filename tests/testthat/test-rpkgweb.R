context("rpkgweb")

test_that("dirname = pkgname", {
  expect_error(rpkgweb("test_web_dirname"), "directory names .* package names")
  expect_error(rpkgweb("test_web_dirname"), "AA != A")
  expect_error(rpkgweb("test_web_dirname"), "BBB != B")
  expect_error(rpkgweb("test_web_dirname"), "DD != D")
})

test_that("must specify root, not subdir in root", {
  test_web_path <- normalize_path("test_web")

  web <- test_web_path %>% rpkgweb
  web %>% root_dir %>% expect_equal(test_web_path)
  web <- test_web_path %>% file.path("AA") %>% rpkgweb
  web %>% root_dir %>% expect_equal(test_web_path %>% file.path("AA"))
  web %>% length %>% expect_equal(0)
})
