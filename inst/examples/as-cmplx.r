# pick the simplicial complex from a TDA filtration object
t <- 2 * pi * c(0, 1, 3, 6) / 7
rf <- TDA::ripsFiltration(
  cbind(x = cos(t), y = sin(t)),
  maxdimension = 2L, maxscale = 1.7
)
print(rf$cmplx)
cp_rf <- as_cmplx(rf)
print(cp_rf)

# convert a simplextree object
st <- simplextree::simplex_tree()
st$insert(list(1:3, 4:5, 6))
cp_st <- as_cmplx(st)
print(cp_st)

# convert an igraph object
g <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
plot(g)
cp_g <- as_cmplx(g)
print(cp_g)

# convert a network object
el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
n <- network::network.edgelist(el, network::network.initialize(4))
plot(n)
cp_n <- as_cmplx(n)
print(cp_n)
