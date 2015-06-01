#' Read a package web
#'
#' This function reads a web of packages.
#'
#' @param web_root The root directory of the package web
#' @param x An object
#'
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @export
rpkgweb <- function(web_root = get_web_root()) {
  dirs <-
    dir(path = web_root, include.dirs = TRUE, full.names = TRUE) %>%
    file.info %>%
    subset(isdir) %>%
    row.names

  lapply(
    dirs %>% setNames(., basename(.)),
    as.package
  ) %>%
    structure(class = "rpkgweb")
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
    paste("A package web consisting of", length(x), "package(s):"),
    paste("-", paste(lapply(x, `[[`, "package"), collapse = ", "))
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

  # Normalise path and strip trailing slashes
  x <- normalise_path(x)
  x <- package_root(x) %||% x

  if (!file.exists(x)) {
    stop("Can't find directory ", x, call. = FALSE)
  }
  if (!file.info(x)$isdir) {
    stop(x, " is not a directory", call. = FALSE)
  }

  x
}

package_root <- function(path) {
  if (is.package(path)) {
    return(path$path)
  }
  stopifnot(is.character(path))

  has_description <- function(path) {
    file.exists(file.path(path, 'DESCRIPTION'))
  }
  path <- normalizePath(path, mustWork = FALSE)
  while (!has_description(path) && !is_root(path)) {
    path <- dirname(path)
  }

  if (is_root(path)) {
    NULL
  } else {
    path
  }
}

is_root <- function(path) {
  identical(path, dirname(path))
}

normalise_path <- function(x) {
  x <- sub("\\\\+$", "/", x)
  x <- sub("/*$", "", x)
  x
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


