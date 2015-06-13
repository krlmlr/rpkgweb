#' Read a package web
#'
#' This function reads a web of packages.
#'
#' @inheritParams makify
#'
#' @importFrom magrittr %>%
#' @importFrom MakefileR makefile
#' @export
write_makefile <- function(web = rpkgweb(), target_dir = NULL) {
  web <- as.rpkgweb(web)

  target_dir <- get_target_dir(web, target_dir)

  makefile_text <-
    web %>%
    makify(target_dir = target_dir) %>%
    makefile %>%
    format

  makefile_name <- file.path(target_dir, "Makefile")
  old_makefile_text <- if (file.exists(makefile_name)) readLines(makefile_name)

  if (!isTRUE(all.equal(makefile_text, old_makefile_text))) {
    writeLines(makefile_text, makefile_name)
  } else {
    message("Contents of ", makefile_name, " unchanged.")
  }
}

get_target_dir <- function(web, target_dir) {
  if (is.null(target_dir)) {
    root_dir(web)
  } else {
    normalize_path(target_dir)
  }
}
