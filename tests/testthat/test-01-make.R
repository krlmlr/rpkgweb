context("make")

test_that("dry run for default Makefile", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, dry_run = TRUE)

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("dry run for Makefile in cwd", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, target_dir = ".", dry_run = TRUE)

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("dry run for Makefile in other dir", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, target_dir = "unrelated", dry_run = TRUE)

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("execution of Makefile for temp lib", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  with_temp_lib(test_make(web))

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("execution of Makefile for other lib", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, lib_dir = "unrelated")

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("execution of Makefile for different target_dir and other lib", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, target_dir = "unrelated", lib_dir = "unrelated")

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})
