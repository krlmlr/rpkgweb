#' Read a package web
#'
#' This function reads a web of packages.
#'
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @importFrom devtools as.package
#' @export
read_web <- function(web = get_web_root()) {
  dirs <-
    dir(path = web, include.dirs = TRUE, full.names = TRUE) %>%
    file.info %>%
    subset(isdir) %>%
    row.names

  lapply(
    dirs %>% setNames(., basename(.)),
    as.package
  ) %>%
    structure(class = "rpkgweb")
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
