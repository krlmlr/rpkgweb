#' Construct a Makefile
#'
#' This function constructs a \code{Makefile} for various objects.
#'
#' @param makefile A Makefile as created by \code{\link[MakefileR]{create_makefile}}
#' @param y The object to construct the \code{Makefile} for
#' @return A Makefile
#'
#' @export
makify <- function(makefile, y) UseMethod("makify", y)

#' @export
makify.default <- function(makefile, y) {
  stop("Cannot use makify for object of class ", class(y))
}

#' @importFrom MakefileR append_make_rule
#' @export
makify.rpkgweb <- function(makefile, y) {
  makefile %>%
    append_make_rule("all", y %>% names) %>%
    append_make_rule(".FORCE") %>%
    append_make_rule("Makefile", ".FORCE", "Rscript -e \"rpkgweb::write_makefile()\"") %>%
    Reduce(y %>% names, ., f = function(m, x)
      append_make_rule(m, x, lib_desc_path(x))) %>%
    Reduce(y %>% names, ., f = function(m, x)
      append_make_rule(m, lib_desc_path(x), code_desc_path(x),
                       c("Rscript -e \"rpkgweb::check_up('$(patsubst %/,%,$(dir $<))')\"",
                         "touch $@"))) %>%
    makify(y %>% deps_df)
}

#' @importFrom MakefileR create_make_rule
#' @export
makify.deps_df <- function(makefile, y) {
  rules <- mapply(y$package %>% lib_desc_path,
                  y$name %>% lib_desc_path,
                  FUN = create_make_rule, SIMPLIFY = FALSE)
  Reduce(c, rules, init = makefile)
}

lib_desc_path <- . %>% file.path(.libPaths()[[1L]], ., "DESCRIPTION")
code_desc_path <- . %>% file.path(., "DESCRIPTION")
