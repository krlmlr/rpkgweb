## This module has been extracted from the devtools package, Git revision
## 7d12dd83f032f6f5861a6fa8a76f717787dd150b (Fri May 29 16:53:03 2015 -0500).
## Reason: Avoid loading of heavy devtools package for analyzing dependencies.
##
## Original files: R/package.r and R/package-deps.r
##
## Original source URL:
## https://github.com/hadley/devtools/tree/7d12dd83f032f6f5861a6fa8a76f717787dd150b/R
##
## Copyright RStudio, Inc.; licensed under GPL v2, license text under
## http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt .
##
## Changes:
## - Don't export symbols
## - Reimplement check_dir using rprojroot package

#' Coerce input to a package.
#'
#' Possible specifications of package:
#' \itemize{
#'   \item path
#'   \item package object
#' }
#' @param x object to coerce to a package
#' @keywords internal
as.package <- function(x = NULL) {
  if (is.package(x)) return(x)

  x <- check_dir(x)
  load_pkg_description(x)
}

#' @importFrom rprojroot find_root is_r_package
check_dir <- function(x) {
  find_root(is_r_package, path = path)
}

# Load package DESCRIPTION into convenient form.
load_pkg_description <- function(path) {
  path <- normalizePath(path)
  path_desc <- file.path(path, "DESCRIPTION")

  if (!file.exists(path_desc)) {
    stop("No description at ", path_desc, call. = FALSE)
  }

  desc <- as.list(read.dcf(path_desc)[1, ])
  names(desc) <- tolower(names(desc))
  desc$path <- path

  structure(desc, class = "package")
}


#' Is the object a package?
#'
#' @keywords internal
is.package <- function(x) inherits(x, "package")



#' Parse package dependency strings.
#'
#' @param string to parse. Should look like \code{"R (>= 3.0), ggplot2"} etc.
#' @return list of two character vectors: \code{name} package names,
#'   and \code{version} package versions. If version is not specified,
#'   it will be stored as NA.
#' @keywords internal
#' @examples
#' parse_deps("httr (< 2.1),\nRCurl (>= 3)")
#' # only package dependencies are returned
#' parse_deps("utils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, ",")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare  <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna   <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if(!all(compare_valid)) {
    stop("Invalid comparison operator in dependency: ",
      paste(compare_nna[!compare_valid], collapse = ", "))
  }

  deps <- data.frame(name = names, compare = compare,
    version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}


#' Check that the version of an imported package satisfies the requirements
#'
#' @param dep_name The name of the package with objects to import
#' @param dep_ver The version of the package
#' @param dep_compare The comparison operator to use to check the version
#' @keywords internal
check_dep_version <- function(dep_name, dep_ver = NA, dep_compare = NA) {
  if (!requireNamespace(dep_name, quietly = TRUE)) {
    stop("Dependency package ", dep_name, " not available.")
  }

  if (xor(is.na(dep_ver), is.na(dep_compare))) {
    stop("dep_ver and dep_compare must be both NA or both non-NA")

  } else if(!is.na(dep_ver) && !is.na(dep_compare)) {

    compare <- match.fun(dep_compare)
    if (!compare(
      as.numeric_version(getNamespaceVersion(dep_name)),
      as.numeric_version(dep_ver))) {

      warning("Need ", dep_name, " ", dep_compare,
        " ", dep_ver,
        " but loaded version is ", getNamespaceVersion(dep_name))
    }
  }
  return(TRUE)
}
