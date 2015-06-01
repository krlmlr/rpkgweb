as.igraph.deps_df <- function(x, ...) {
  igraph::graph.data.frame(x[c("name", "package", "dep_type")])
}

get_dep_depth_df <- function(pkg, web) {
  deps <- web %>%
    deps_df

  deps_igraph <-
    deps %>%
    as.igraph.deps_df %>%
    igraph::graph.union(igraph::graph.empty(1) %>%
                          igraph::set.vertex.attribute("name", value = pkg$package))

  graph_dep_dist <-
    deps_igraph %>%
    igraph::graph.bfs(pkg$package, unreachable = FALSE, dist = TRUE) %>%
    extract2("dist")

  packages <- deps_igraph %>%
    igraph::get.vertex.attribute("name") %>%
    extract(!is.nan(graph_dep_dist))
  depth <- graph_dep_dist[!is.nan(graph_dep_dist)]

  data.frame(packages, depth)
}
