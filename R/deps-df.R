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
    web$packages %>%
    lapply(names) %>%
    lapply(intersect, c("depends", "imports", "suggests")) %>%
    mapply(web$packages, FUN = function(names, webitem) {
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
