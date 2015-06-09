# While not in devtools or new package
with_temp_libpaths <- function(code) {
  lib_paths <- tempfile("lib", fileext = "rpkgweb")
  dir.create(lib_paths)
  devtools::with_libpaths(lib_paths, code)
}
