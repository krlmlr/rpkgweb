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
    devtools::as.package
  ) %>%
    structure(class = "rpkgweb")
}
