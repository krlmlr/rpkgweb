context("make")


web <- rpkgweb("test_web")

test_that("execution of Makefile for different target_dir and other lib", {
  test_make(web, target_dir = "unrelated", lib_dir = "unrelated/lib")
})
