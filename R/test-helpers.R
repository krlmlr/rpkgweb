skip_if_packages_installed <- function(web) {
  # Packages are originally not installed
  if (any((web %>% names) %in% rownames(installed.packages()))) {
    skip("At least one of the test packages is installed")
  }
}

safe_unload <- function(pkg) {
  if (pkg$package %in% loadedNamespaces()) devtools::unload(pkg)
}
