context("'network' objects")

data(flo, package = "network")
nw_flo <- network::network(flo, directed = FALSE)

test_that("'network'-to-'graph' conversion preserves 0- and 1-simplices", {
  ig_flo <- as_igraph(nw_flo)
  expect_equal(network::network.size(nw_flo), igraph::gorder(ig_flo))
  expect_true(all(network::as.edgelist(nw_flo) == igraph::as_edgelist(ig_flo)))
})

test_that("'network'-to-'igraph' conversion preserves attributes", {
  v_id <- 1L + sample(network::network.size(nw_flo))
  nw_flo <- network::set.vertex.attribute(nw_flo, "id", v_id)
  stopifnot("id" %in% network::list.vertex.attributes(nw_flo))
  ig_flo <- as_igraph(nw_flo)
  expect_true("id" %in% igraph::vertex_attr_names(ig_flo))
})

test_that("'network'-to-list conversion preserves 0- and 1-simplices", {
  st_flo <- as_simplextree(nw_flo)
  expect_equal(network::network.size(nw_flo), st_flo$n_simplices[[1L]])
  expect_true(all(network::as.edgelist(nw_flo) == st_flo$edges))
})

test_that("'network'-to-list conversion preserves 0- and 1-simplices", {
  cp_flo <- as_cmplx(nw_flo)
  expect_equal(network::network.size(nw_flo), length(unique(unlist(cp_flo))))
  expect_true(all(
    network::as.edgelist(nw_flo) ==
      t(sapply(cp_flo[as.logical(sapply(cp_flo, length) - 1)], identity))
  ))
})
