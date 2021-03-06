#' Return dependencies in a package web as data frame
#'
#' This function computes all dependencies (internal and external) in a
#' package web and returns the information as a data frame.
#'
#' @inheritParams check_up
#'
#' @importFrom devtools parse_deps
#' @export
deps_df <- function(web = rpkgweb()) {
  web <- as.rpkgweb(web)

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
    SIMPLIFY = FALSE) %>%
    c(list(data.frame(
      package = character(),
      dep_type = character(),
      deps = character())))

  all_deps_df <- do.call(rbind, all_deps)

  lapply(
    seq_len(nrow(all_deps_df)),
    function(i) {
      name <- parse_deps(all_deps_df$deps[[i]])$name
      if (length(name) > 0) {
        data.frame(package = all_deps_df$package[[i]],
                   dep_type = all_deps_df$dep_type[[i]],
                   dep_package = name,
                   stringsAsFactors = FALSE)
      } else {
        NULL
      }
    }
  ) %>%
    c(list(data.frame(
      package = character(),
      dep_type = character(),
      dep_package = character()))) %>%
    do.call(rbind, .) %>%
    transform(internal = dep_package %in% names(all_deps)) %>%
    prepend_class("deps_df")
}

prepend_class <- function(x, new_class) {
  attr(x, "class") <- c(new_class, attr(x, "class", exact = TRUE))
  x
}
