#' Bump package version
#'
#' This function increases the given component of the version number
#' in the DESCRIPTION file of a package.
#'
#' @inheritParams check_up
#' @param component The position of the version to increase, starting at 1.
#'
#' @importFrom devtools as.package
#' @importFrom magrittr %>% extract
#' @export
bump <- function(pkg, component = 2L, format = "0.0-0") {
  pkg <- as.package(pkg)

  desc_path <- file.path(pkg$path, "DESCRIPTION")
  desc <- ("devtools" %:::% "read_dcf")(desc_path)

  desc$Version <-
    desc$Version %>%
    increase_version(component, format)

  ("devtools" %:::% "write_dcf")(desc_path, desc)
  message("Package ", pkg$package, " bumped to version ", desc$Version)
  invisible(NULL)
}

increase_version <- function(version, component, format) {
  sprintf_format <- get_sprintf_format(format, component)

  version <-
    version %>%
    strsplit("[.-]") %>%
    extract2(1L) %>%
    sapply(as.integer) %>%
    extract(seq_len(component))

  version[is.na(version)] <- 0L
  version[[component]] <- version[[component]] + 1L
  do.call(sprintf, c(list(sprintf_format), version))
}

get_sprintf_format <- function(format, component) {
  format_split <- format %>%
    strsplit("0", fixed = TRUE) %>%
    extract2(1L) %>%
    extract(-1L)

  component_split_len <- component - 1L
  format_split_len_diff <- length(format_split) - component_split_len
  if (format_split_len_diff < 0L) {
    format_split <- c(format_split, rep(".", -format_split_len_diff))
  } else {
    format_split <- format_split[seq_len(component_split_len)]
  }

  paste(c("", format_split, ""), collapse = "%s")
}

"%:::%" <- function(p, f) {
  get(f, envir = asNamespace(p))
}
