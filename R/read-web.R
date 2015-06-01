#' Read a package web
#'
#' This function reads a web of packages.
#'
#' @param root_dir The root directory of the package web
#' @param x An object
#'
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @export
rpkgweb <- function(root_dir = get_web_root()) {
  dirs <-
    dir(path = root_dir, include.dirs = TRUE, full.names = TRUE) %>%
    file.info %>%
    subset(isdir) %>%
    row.names

  structure(
    list(
      packages = lapply(
        dirs %>% setNames(., basename(.)),
        as.package
      ),
      root_dir = root_dir
    ),
    class = "rpkgweb")
}

#' @rdname rpkgweb
#' @export
is.rpkgweb <- function(x) inherits(x, "rpkgweb")

#' @rdname rpkgweb
#' @export
as.rpkgweb <- function(x) UseMethod("as.rpkgweb", x)

#' @export
as.rpkgweb.default <- function(x) {
  stop("Required character value or object of class rpkgweb.", call. = FALSE)
}

#' @export
as.rpkgweb.rpkgweb <- identity

#' @export
as.rpkgweb.character <- function(x) {
  rpkgweb(web_root = x)
}

#' @export
format.rpkgweb <- function(x, ...) {
  c(
    paste("A package web rooted at", x$root_dir, "consisting of",
          length(x$packages), "package(s):"),
    paste("-", paste(lapply(x$packages, `[[`, "package"), collapse = ", "))
  )
}

#' @export
print.rpkgweb <- function(x, ...) {
  cat(paste(format(x), collapse = "\n"), "\n")
}


## as.package et al from devtools@dc61a9e79c44f4 to avoid importing it

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


check_dir <- function(x) {
  if (is.null(x)) {
    stop("Path is null", call. = FALSE)
  }

  package_root(x)
}

#' @importFrom rprojroot find_root is_r_package
package_root <- function(path) {
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
