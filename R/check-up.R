#' Check and install package if required
#'
#' If the currently installed version of a package differs
#' from the locally available version, this function checks the package
#' and installs it if all checks succeed.
#'
#' @importFrom devtools as.package
#' @importFrom magrittr %>% extract2
#' @param pkg Location of package
#' @export
check_up <- function(pkg) {
  pkg_path <- find.package(pkg, quiet = TRUE)
  installed <- if (length(pkg_path) > 0L) {
    as.package(pkg_path)
  } else {
    NULL
  }

  available <- read_web() %>%
    extract2(pkg)

  cmp <- compareVersion(installed$version %||% "", available$version)
  if (cmp == 0) {
    message("Package ", pkg, " is up to date: ", available$version)
    return(invisible(NULL))
  }

  devtools::check(available$path)
  devtools::install(available$path)
  message("Package ", pkg, " updated: ", available$version)
  return(invisible(NULL))
}

"%||%" <- function(a, b) if (!is.null(a)) a else b
