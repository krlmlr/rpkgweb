#' Bump package version
#'
#' This function increases the given component of the version number
#' in the DESCRIPTION file of a package.
#'
#' @inheritParams devtools::install
#' @inheritParams check_up
#' @param component The position of the version to increase, starting at 1.
#'
#' @importFrom magrittr %>% extract
#' @importFrom devtools as.package
#' @export
bump <- function(pkg = ".", component = 2L, format = "0.0-0") {
  pkg <- as.package(pkg)

  desc_path <- file.path(pkg$path, "DESCRIPTION")
  desc <- read_dcf(desc_path)

  desc$Version <-
    desc$Version %>%
    increase_version(component, format)

  write_dcf(desc_path, desc)
  message("Package ", pkg$package, " bumped to version ", desc$Version)
  invisible(desc$Version)
}

# Minimum number of version components
.MIN_COMPONENTS <- 2L

increase_version <- function(version, component, format) {
  sprintf_format <- get_sprintf_format(format, component)

  version <-
    version %>%
    strsplit("[.-]") %>%
    extract2(1L) %>%
    sapply(as.integer) %>%
    extract(seq_len(component)) %>%
    extract(seq_len(max(component, .MIN_COMPONENTS)))

  version[is.na(version)] <- 0L
  version[[component]] <- version[[component]] + 1L
  do.call(sprintf, c(list(sprintf_format), version))
}

#' @importFrom magrittr %>% extract2
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
    format_split <- format_split[seq_len(max(component_split_len,
                                             .MIN_COMPONENTS - 1L))]
  }

  paste(c("", format_split, ""), collapse = "%s")
}

read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}

write_dcf <- function(path, desc) {
  desc <- unlist(desc)
  # Add back in continuation characters
  desc <- gsub("\n[ \t]*\n", "\n .\n ", desc, perl = TRUE, useBytes = TRUE)
  desc <- gsub("\n \\.([^\n])", "\n  .\\1", desc, perl = TRUE, useBytes = TRUE)
  desc <- gsub("^([^\n])", " \\1", desc, perl = TRUE, useBytes = TRUE)

  text <- paste0(names(desc), ":", desc, collapse = "\n")
  text <- gsub("\n*$", "\n", text, perl = TRUE, useBytes = TRUE)

  cat(text, file = path)
}
