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
  makefile_text <-
    web %>%
    read_web %>%
    makify(create_makefile(), .) %>%
    format

  makefile_name <- file.path(web, "Makefile")
  if (!isTRUE(all.equal(makefile_text, readLines(makefile_name)))) {
    writeLines(makefile_text, makefile_name)
  } else {
    message("Contents of ", makefile_name, " unchanged.")
  }
}
