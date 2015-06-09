# While not in devtools or new package
with_temp_lib <- function(code) {
  lib_paths <- tempfile("lib", fileext = "rpkgweb")
  dir.create(lib_paths)

  all_lib_paths <- c(lib_paths, .libPaths())

  devtools::with_libpaths(all_lib_paths, code)
}
