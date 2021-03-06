#' Read a package web
#'
#' An R package web is a collection of R packages stored in the same directory.
#' These function are related to the S3 class \code{"rpkgweb"}.
#'
#' The \code{rpkgweb} function constructs an \code{"rpkgweb"} object
#' for a given directory.
#'
#' @param root_dir The root directory of the package web
#' @param x An object
#'
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @importFrom devtools as.package
#' @export
rpkgweb <- function(root_dir = get_web_root()) {
  dirs <-
    dir(path = root_dir, include.dirs = TRUE, full.names = TRUE) %>%
    file.info %>%
    subset(isdir) %>%
    row.names

  desc_dirs <-
    dir(path = dirs, pattern = "^DESCRIPTION$", full.names = TRUE) %>%
    dirname

  packages <-
    desc_dirs %>%
    lapply(as.package) %>%
    { setNames(., nm = lapply(., `[[`, "package")) }

  differing <- which(names(packages) != basename(desc_dirs))
  if (length(differing) > 0L) {
    stop("Implicit make rules require that the directory names match the package names.\n",
         "This is not true for:\n",
         paste(
           sprintf(
             "%s != %s",
             names(packages[differing]),
             basename(desc_dirs[differing])),
           collapse = "\n"),
         call. = FALSE
    )
  }

  structure(
    packages,
    class = "rpkgweb",
    root_dir = normalize_path(root_dir)
  )
}

normalize_path <- function(path) {
  normalizePath(path, winslash = "/")
}

#' @rdname rpkgweb
#' @details
#'   \code{is.rpkgweb} checks if an object is of class \code{"rpkgweb"}.
#' @export
is.rpkgweb <- function(x) inherits(x, "rpkgweb")

#' @rdname rpkgweb
#' @details
#'   All functions that operate on a package web will call \code{as.rpkgweb} on
#'   the input. Objects of class \code{"rpkgweb"} are passed through,
#'   \code{character} values are coerced using the \code{rpkgweb} function
#'   (i.e., they are interpreted as root path of the package web);
#'   other inputs will raise an error.
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
  rpkgweb(root_dir = x)
}

#' @export
format.rpkgweb <- function(x, ...) {
  rooted <- paste("A package web rooted at", root_dir(x))
  if (length(x) == 0) {
    paste(rooted, "without any packages.")
  } else {
    c(
      paste(rooted, "consisting of",
            length(x), "package(s):"),
      paste("-", paste(lapply(x, `[[`, "package"), collapse = ", "))
    )
  }
}

#' @export
print.rpkgweb <- function(x, ...) {
  cat(paste(format(x), collapse = "\n"), "\n")
}

#' @details \code{root_dir} returns the root directory of a package web.
#' @rdname rpkgweb
#' @export
root_dir <- function(x) UseMethod("root_dir", x)

#' @export
root_dir.default <- function(x) {
  stop("Required object of class rpkgweb.", call. = FALSE)
}

#' @export
root_dir.rpkgweb <- function(x) {
  attr(x, "root_dir", TRUE)
}
