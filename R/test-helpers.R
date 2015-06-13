skip_if_packages_installed <- function(web) {
  # Packages are originally not installed
  if (any((web %>% names) %in% rownames(installed.packages()))) {
    skip("At least one of the test packages is installed")
  }
}

safe_unload <- function(pkg) {
  if (pkg$package %in% loadedNamespaces()) devtools::unload(pkg)
}

envvar <- function() {
  # Instruct the Makefile where to load the package from
  pkg_path <- subset(devtools::loaded_packages(), package == "rpkgweb")$path
  ret <- if (file.path(pkg_path, "tests", "testthat") == normalizePath(".", winslash = "/")) {
    # Loaded by devtools, need patched version
    list(RPKGWEB_QUALIFY = sprintf("devtools::load_all('%s');", pkg_path))
  } else {
    # Installed package in R CMD check -- reset R_TESTS which points to an invalid path
    list(R_TESTS = "")
  }
  ret <- c(ret, R_LIBS=paste(.libPaths(), collapse = ":"))
  ret
}
