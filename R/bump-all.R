#' Bump package version and all dependent versions
#'
#' This function increases the given component of the version number
#' in the DESCRIPTION file of a package, and propagates this increase
#' to all reverse dependencies.
#'
#' @inheritParams check_up
#' @inheritParams bump
#'
#' @importFrom magrittr %>% extract extract2
#' @importFrom igraph as.igraph vertex graph.bfs graph.union graph.empty
#'   get.vertex.attribute set.vertex.attribute
#' @export
bump_all <- function(pkg, webroot = get_web_root(), component = 2L, format = "0.0-0") {
  pkg <- as.package(pkg)
  web <- read_web(webroot)

  dep_depth <- get_dep_depth_df(pkg, web)

  dep_depth %>%
    dplyr::arrange(depth) %>%
    dplyr::group_by(packages) %>%
    dplyr::do(
      new_version = bump(.$package, component = component + .$depth, format = format)
    ) %>%
    dplyr::ungroup %>%
    invisible
}

#' @importFrom igraph graph.data.frame
#' @export
as.igraph.deps_df <- function(x, ...) {
  graph.data.frame(x[c("name", "package", "dep_type")])
}

get_dep_depth_df <- function(pkg, web) {
  deps <- web %>%
    deps_df

  deps_igraph <-
    deps %>%
    as.igraph %>%
    graph.union(graph.empty(1) %>% set.vertex.attribute("name", value = pkg$package))

  graph_dep_dist <-
    deps_igraph %>%
    graph.bfs(pkg$package, unreachable = FALSE, dist = TRUE) %>%
    extract2("dist")

  packages <- deps_igraph %>%
    get.vertex.attribute("name") %>%
    extract(!is.nan(graph_dep_dist))
  depth <- graph_dep_dist[!is.nan(graph_dep_dist)]

  data.frame(packages, depth)
}
