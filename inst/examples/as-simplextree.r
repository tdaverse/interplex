# convert a TDA filtration object
t <- 2 * pi * c(0, 1, 3, 6) / 7
rf <- TDA::ripsFiltration(
  cbind(x = cos(t), y = sin(t)),
  maxdimension = 2L, maxscale = 1.7
)
print(rf$cmplx)
st_rf <- as_simplextree(rf)
print(st_rf)
st_rf2 <- as_simplextree(rf$cmplx)
print(st_rf2)

# convert an igraph object
g <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
print(g)
st_g <- as_simplextree(g)
print(st_g)

# specify 0-simplex indices
set.seed(0)
g <- igraph::set_vertex_attr(g, "id", value = sample(igraph::vcount(g)) + 1L)
igraph::V(g)$id
igraph::as_edgelist(g)
st_g <- as_simplextree(g, index = "id")
st_g$vertices
st_g$edges

# convert a network object
el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
n <- network::network.edgelist(el, network::network.initialize(4))
print(n)
st_n <- as_simplextree(n)
print(st_n)
