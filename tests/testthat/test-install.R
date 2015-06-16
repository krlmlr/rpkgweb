context("install")

test_that("installation updates directory timestamp", {
  # This is a prerequisite for "dummy file" rules
  with_temp_lib({
    lib_dir <- .libPaths()[[1L]]
    timestamp <- file_info(lib_dir)$mtime

    devtools::install("test_web/AA", quiet = TRUE)
    expect_less_than(timestamp, file_info(file.path(lib_dir, "AA"))$mtime)
    expect_less_than(timestamp, file_info(lib_dir)$mtime)
    timestamp <- file_info(lib_dir)$mtime

    devtools::uninstall("test_web/AA")
    expect_less_than(timestamp, file_info(lib_dir)$mtime)
    timestamp <- file_info(lib_dir)$mtime
  })
})
