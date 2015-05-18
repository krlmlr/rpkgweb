#' Read a package web
#'
#' This function reads a web of packages.
#'
#' @inheritParams read_web
#'
#' @importFrom magrittr %>%
#' @importFrom MakefileR create_makefile
#' @export
write_makefile <- function(web = get_web_root()) {
  web %>%
    read_web %>%
    makify(create_makefile(), .) %>%
    format %>%
    writeLines(file.path(web, "Makefile"))
}
