\dontrun{

if (requireNamespace("reticulate", quietly = TRUE)) {
  # print GUDHI simplices
  # TODO: print filtration value beside each simplex
  print_py_gudhi <- function(x) {
    reticulate::iterate(
      x$get_skeleton(x$dimension()),
      function(s) print(s[[1]]),
      simplify = FALSE
    )
  }
}

if (requireNamespace("TDA", quietly = TRUE)) {
  # convert a TDA filtration object
  t <- 2 * pi * c(0, 1, 3, 6) / 7
  rf <- TDA::ripsFiltration(
    cbind(x = cos(t), y = sin(t)),
    maxdimension = 2L, maxscale = 1.7
  )
  print(rf$cmplx)
  gd_rf <- as_py_gudhi_simplextree(rf)
  print_py_gudhi(gd_rf)
  # can no longer pass the `cmplx` element alone, but can reformat it
  tda_rf <- as_tda_filtration(rf$cmplx)
  # gd_rf2 <- as_py_gudhi_simplextree(rf$cmplx)
  gd_rf2 <- as_py_gudhi_simplextree(tda_rf)
  print_py_gudhi(gd_rf2)
}

if (requireNamespace("simplextree", quietly = TRUE)) {
  # convert a simplextree simplicial complex
  st <- simplextree::simplex_tree()
  st$insert(list(3:5, 5:6, 8))
  gd_st <- as_py_gudhi_simplextree(st)
  print_py_gudhi(gd_st)
  
  # convert a simplextree filtration
  d <- dist(cbind(c(0, 1.5, 0), c(0, 0, 2.75)))
  sf <- simplextree::rips(d, eps = 3, filtered = TRUE)
  gd_sf <- as_py_gudhi_simplextree(sf)
  print_py_gudhi(gd_sf)
}

if (requireNamespace("igraph", quietly = TRUE)) {
  # convert an igraph object
  ig <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
  print(ig)
  gd_ig <- as_py_gudhi_simplextree(ig)
  print_py_gudhi(gd_ig)
  
  # specify 0-simplex indices
  set.seed(0L)
  ig <- igraph::set_vertex_attr(ig, "id", value = sample(igraph::vcount(ig)) + 1L)
  igraph::V(ig)$id
  igraph::as_edgelist(ig)
  gd_ig2 <- as_py_gudhi_simplextree(ig, index = "id")
  print_py_gudhi(gd_ig2)
  
  # specify filtration values
  ig <- igraph::set.vertex.attribute(ig, "filt", value = c(0, .1, .1, .2))
  ig <- igraph::set.edge.attribute(ig, "filt", value = c(.1, .2, .2, .3))
  gd_ig3 <- as_py_gudhi_simplextree(ig, index = "id", value = "filt")
  print_py_gudhi(gd_ig3)
  
  # convert a network object
  el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
  nw <- network::network.edgelist(el, network::network.initialize(4))
  print(nw)
  gd_nw <- as_py_gudhi_simplextree(nw)
  print_py_gudhi(gd_nw)
  
  # specify indices and values
  set.seed(0L)
  nw <- network::set.vertex.attribute(nw, "id", sample(igraph::vcount(ig)) + 1L)
  nw <- network::set.vertex.attribute(nw, "filt", c(0, .1, .1, .2))
  nw <- network::set.edge.attribute(nw, "filt", c(.1, .2, .2, .3))
  gd_nw2 <- as_py_gudhi_simplextree(nw, index = "id", value = "filt")
  print_py_gudhi(gd_nw2)
}

if (requireNamespace("network", quietly = TRUE)) {
  # convert a network object
  el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
  nw <- network::network.edgelist(el, network::network.initialize(4))
  print(nw)
  gd_nw <- as_py_gudhi_simplextree(nw)
  print_py_gudhi(gd_nw)
}
}
