context("make")


web <- rpkgweb("test_web")

test_that("dry run for default Makefile", {
  test_make(web, dry_run = TRUE)
})

test_that("dry run for Makefile in cwd", {
  test_make(web, target_dir = ".", dry_run = TRUE)
})

test_that("dry run for Makefile in other dir", {
  test_make(web, target_dir = "unrelated", dry_run = TRUE)
})

test_that("execution of Makefile for temp lib", {
  with_temp_lib(test_make(web))

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("execution of Makefile for other lib", {
  test_make(web, lib_dir = "unrelated")
})

test_that("execution of Makefile for different target_dir and lib in target_dir", {
  # Not recommended!
  test_make(web, target_dir = "unrelated", lib_dir = "unrelated")
})

test_that("execution of Makefile for different target_dir and other lib", {
  test_make(web, target_dir = "unrelated", lib_dir = "unrelated/lib")
})
