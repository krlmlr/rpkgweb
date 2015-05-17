#' Construct a Makefile
#'
#' This function constructs a \code{Makefile} for various objects.
#'
#' @param x The object to construct the \code{Makefile} for
#' @return A character vector that represents the contents of the generated
#' \code{Makefile}
#'
#' @export
makify <- function(x) UseMethod("makify", x)

#' @export
makify.default <- function(x) {
  stop("Cannot use makify for object of class ", class(x))
}

#' @export
makify.rpkgweb <- function(x) {
  c(
    create_make_rule("all", x %>% names, space = ""),
    create_make_rule(".FORCE", NULL),
    create_make_rule(x %>% names, ".FORCE", space = ""),
    makify(x %>% deps_df)
  )
}

#' @export
makify.deps_df <- function(x) {
  mapply(x$package, x$name, FUN = create_make_rule)
}
