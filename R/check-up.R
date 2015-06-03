#' Check and install package if required
#'
#' If the currently installed version of a package differs
#' from the locally available version, this function uninstalls all reverse
#' dependencies, checks the package and installs it if all checks succeed.
#'
#' TODO: devtools-like interface with pkg = "." and web = rpkgweb(pkg)
#'
#' TODO: argument unload = !is_loaded(pkg)
#'
#' @return \code{TRUE} if package has been updated, \code{FALSE} if the package
#'   is already up to date, or an error if there was a failure
#'
#' @param pkg_name Name of the package, \emph{not} the path!
#' @importFrom magrittr %>%
#' @importFrom devtools as.package
#' @export
check_up <- function(pkg_name, web = rpkgweb(), quiet = FALSE) {
  web <- as.rpkgweb(web)

  if (quiet) message <- function(...) invisible(NULL)

  available <- web$packages[[pkg_name]]

  installed <- get_installed(available$package)
  installed_version <- installed$version %||% ""

  cmp <- compareVersion(installed_version, available$version)
  if (cmp == 0) {
    message("Package ", available$package, " is up to date: ", available$version)
    installed_description_file <- file.path(installed$path, "DESCRIPTION")
    message("Updating timestamp for ", installed_description_file)
    Sys.setFileTime(installed_description_file, Sys.time())
    return(invisible(FALSE))
  }

  if (installed_version == "") {
    message("Package ", available$package, " not installed.")
  } else {
    message("Package ", available$package, " installed in version ", installed_version,
            ", now installing ", available$version)
  }

  ##devtools::check(available, cran = FALSE)

  depth_df <- get_dep_depth_df(available, web)
  pkgs_to_remove <-
    depth_df$package %>%
    find.package(quiet = TRUE) %>%
    basename

  if (length(pkgs_to_remove) > 0) {
    message("Removing packages: ", paste(pkgs_to_remove, collapse = ", "))
    remove.packages(pkgs_to_remove, .libPaths()[[1L]])
  }

  if (devtools:::uses_testthat(available)) {
    devtools::test(available, quiet = quiet, reporter = "stop")
  }

  devtools::install(available, dependencies = FALSE,
                    args = "--no-test-load", quiet = quiet)

  if (compareVersion(get_installed_version(available$package), available$version) != 0) {
    stop("Package ", available$package, " not updated")
  }
  message("Package ", available$package, " ", "updated", ": ", available$version)
  return(invisible(TRUE))
}

#' @importFrom devtools as.package
get_installed <- function(pkg) {
  pkg_path <- find.package(pkg, quiet = TRUE)
  if (length(pkg_path) > 0L) {
    as.package(pkg_path)
  } else {
    NULL
  }
}

"%||%" <- function(a, b) if (!is.null(a)) a else b
