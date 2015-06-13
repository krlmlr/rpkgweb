context("make")

test_that("execution of Makefile for different target_dir and other lib", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, target_dir = "unrelated", lib_dir = "unrelated")

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})
