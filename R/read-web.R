#' Read a package web
#'
#' This function reads a web of packages.
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

  structure(
    list(
      packages = lapply(
        desc_dirs %>% setNames(., basename(.)),
        as.package
      ),
      root_dir = normalizePath(root_dir, winslash = "/")
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
  rpkgweb(root_dir = x)
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
