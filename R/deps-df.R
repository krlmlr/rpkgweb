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
#' @importFrom dplyr bind_rows group_by do ungroup
#' @importFrom devtools parse_deps
deps_df.rpkgweb <- function(web) {
  web %>%
    lapply(names) %>%
    lapply(intersect, c("depends", "imports", "suggests")) %>%
    mapply(web, FUN = function(names, webitem) {
      data.frame(package = webitem$package, dep_type = names,
                 deps = unname(unlist(webitem[names])),
                 stringsAsFactors = FALSE)
    },
    SIMPLIFY = FALSE) %>%
    bind_rows %>%
    group_by(package, dep_type) %>%
    do(parse_deps(.$deps %>% debug_pipe)[, "name", drop = FALSE]) %>%
    ungroup %>%
    prepend_class("deps_df")
}

prepend_class <- function(x, new_class) {
  attr(x, "class") <- c(new_class, attr(x, "class", exact = TRUE))
  x
}