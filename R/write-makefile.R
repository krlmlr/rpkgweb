#' Read a package web
#'
#' This function reads a web of packages.
#'
#' @importFrom magrittr %>%
#' @importFrom MakefileR create_makefile
#' @export
write_makefile <- function(web = rpkgweb()) {
  web <- as.rpkgweb(web)

  makefile_text <-
    web %>%
    makify(create_makefile(), .) %>%
    format

  makefile_name <- file.path(web$root_dir, "Makefile")
  old_makefile_text <- if (file.exists(makefile_name)) readLines(makefile_name)

  if (!isTRUE(all.equal(makefile_text, old_makefile_text))) {
    writeLines(makefile_text, makefile_name)
  } else {
    message("Contents of ", makefile_name, " unchanged.")
  }
}
