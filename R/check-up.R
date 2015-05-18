#' Check and install package if required
#'
#' If the currently installed version of a package differs
#' from the locally available version, this function checks the package
#' and installs it if all checks succeed.
#'
#' @importFrom devtools as.package
#' @importFrom magrittr %>%
#' @param pkg Location of package
#' @export
check_up <- function(pkg) {
  installed <- as.package(find.package(pkg))

  available <- read_web() %>%
    extract2(pkg)

  cmp <- compareVersion(installed$version, available$version)
  if (cmp == 0) {
    message("Package ", pkg, " is up to date: ", available$version)
    return()
  }

  devtools::check(available$path)
  devtools::install(available$path)
  message("Package ", pkg, " updated: ", available$version)
  invisible(NULL)
}
