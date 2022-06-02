# convert a TDA filtration object
t <- 2 * pi * c(0, 1, 3, 6) / 7
rf <- TDA::ripsFiltration(
  cbind(x = cos(t), y = sin(t)),
  maxdimension = 2L, maxscale = 1.7
)
print(rf$cmplx)
ig_rf <- as_igraph(rf)
print(ig_rf)
ig_rf2 <- as_igraph(rf$cmplx)
print(ig_rf2)

# convert a simplextree object
st <- simplextree::simplex_tree()
st$insert(list(3:5, 5:6, 8))
ig_st <- as_igraph(st)
print(ig_st)

# convert a network object
el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
n <- network::network.edgelist(el, network::network.initialize(4))
print(n)
ig_n <- as_igraph(n)
print(ig_n)