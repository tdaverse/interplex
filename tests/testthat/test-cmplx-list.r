context("lists in the TDA package `cmplx` list format")

cp_ac <- TDA::alphaComplexFiltration(X = TDA::circleUnif(n = 10L))$cmplx
cp_ac_vc <- length(unique(unlist(cp_ac)))
cp_ac_el <- t(sapply(cp_ac[sapply(cp_ac, length) == 2L], identity))

test_that("list-to-'igraph' conversion preserves vertices", {
  ig_ac <- as_igraph(cp_ac)
  expect_equal(cp_ac_vc, igraph::gorder(ig_ac))
  expect_true(all(sort_el(cp_ac_el) == sort_el(igraph::as_edgelist(ig_ac))))
})

test_that("list-to-'network' conversion preserves vertices", {
  nw_ac <- as_network(cp_ac)
  expect_equal(cp_ac_vc, network::network.size(nw_ac))
  expect_true(all(sort_el(cp_ac_el) == sort_el(network::as.edgelist(nw_ac))))
})

test_that("list-to-'simplextree' conversion preserves 0,1-simplices", {
  st_ac <- as_simplextree(cp_ac)
  expect_equal(cp_ac_vc, st_ac$n_simplices[[1L]])
  expect_true(all(sort_el(cp_ac_el) == st_ac$edges))
})

cp_ac <- lapply(cp_ac, function(s) s + 2L)

test_that("list-to-'igraph' conversion encodes indices", {
  ig_ac <- as_igraph(cp_ac, index = "index")
  expect_true("index" %in% igraph::vertex_attr_names(ig_ac))
})

test_that("list-to-'network' conversion encodes indices", {
  nw_ac <- as_network(cp_ac, index = "index")
  expect_true("index" %in% network::list.vertex.attributes(nw_ac))
})
