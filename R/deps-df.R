#' Return dependencies in a package web as data frame
#'
#' This function computes all dependencies (internal and external) in a
#' package web and returns the information as a data frame.
#'
#' @export
deps_df <- function(web) UseMethod("deps_df")

#' @export
deps_df.default <- function(web) {
  stop("Need object of class rpkgweb")
}

#' @export
deps_df.rpkgweb <- function(web) {
  all_deps <-
    web %>%
    lapply(names) %>%
    lapply(intersect, c("depends", "imports", "suggests")) %>%
    mapply(web, FUN = function(names, webitem) {
      if (length(names) > 0) {
        data.frame(package = webitem$package, dep_type = names,
                   deps = unname(unlist(webitem[names])),
                   stringsAsFactors = FALSE)
      }
    },
    SIMPLIFY = FALSE)

  all_deps %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(package, dep_type) %>%
    dplyr::do(parse_deps(.$deps)[, "name", drop = FALSE]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(name %in% names(all_deps)) %>%
    prepend_class("deps_df")
}

prepend_class <- function(x, new_class) {
  attr(x, "class") <- c(new_class, attr(x, "class", exact = TRUE))
  x
}

#' Parse package dependency strings.
#'
#' Copy from devtools@@dc61a9e79c44f4 to avoid loading devtools to parse
#' dependencies
#'
#' @param string to parse. Should look like \code{"R (>= 3.0), ggplot2"} etc.
#' @return list of two character vectors: \code{name} package names,
#'   and \code{version} package versions. If version is not specified,
#'   it will be stored as NA.
#' @keywords internal
parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, ",")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare  <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna   <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if(!all(compare_valid)) {
    stop("Invalid comparison operator in dependency: ",
         paste(compare_nna[!compare_valid], collapse = ", "))
  }

  deps <- data.frame(name = names, compare = compare,
                     version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}
