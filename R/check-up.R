#' Check and install package if required
#'
#' If the currently installed version of a package differs
#' from the locally available version, this function checks the package
#' and installs it if all checks succeed.
#'
#' @inheritParams devtools::install
#'
#' @importFrom magrittr %>%
#' @importFrom devtools as.package
#' @export
check_up <- function(pkg, web = rpkgweb(), quiet = FALSE) {
  web <- as.rpkgweb(web)

  available <- web$packages[[pkg]]

  installed_version <- get_installed_version(pkg)

  cmp <- compareVersion(installed_version, available$version)
  if (cmp == 0) {
    message("Package ", pkg, " is up to date: ", available$version)
    return(invisible(NULL))
  }

  if (installed_version == "") {
    message("Package ", pkg, " not yet installed.")
  } else {
    message("Package ", pkg, " installed in version ", installed_version,
            ", now installing ", available$version)
  }

  ##devtools::check(available, cran = FALSE)

  depth_df <- get_dep_depth_df(pkg %>% as.package, web)
  pkgs_to_remove <-
    depth_df$package %>%
    find.package(quiet = TRUE) %>%
    basename

  if (length(pkgs_to_remove) > 0) {
    message("Removing packages: ", paste(pkgs_to_remove, collapse = ", "))
    remove.packages(pkgs_to_remove)
  }

  if (devtools:::uses_testthat(available)) {
    test_res <- devtools::test(available, quiet = quiet)
    test_res_df <- as.data.frame(test_res)
    if (any(test_res_df[["error"]])) {
      stop("Error in tests for package ", available$package)
    }
    if (any(test_res_df[["failed"]] > 0L)) {
      stop("Tests failed for package ", available$package)
    }
  }

  devtools::install(available, args = "--no-test-load", quiet = quiet)

  if (compareVersion(get_installed_version(pkg), available$version) != 0) {
    stop("Package ", pkg, " not updated")
  }
  message("Package ", pkg, " ", "updated", ": ", available$version)
  return(invisible(NULL))
}

#' @importFrom devtools as.package
get_installed_version <- function(pkg) {
  pkg_path <- find.package(pkg, quiet = TRUE)
  installed <- if (length(pkg_path) > 0L) {
    as.package(pkg_path)
  } else {
    NULL
  }

  installed$version %||% ""
}

"%||%" <- function(a, b) if (!is.null(a)) a else b
