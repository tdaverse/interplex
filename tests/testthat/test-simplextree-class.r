context("'simplextree' objects")

st_ex <- simplextree::simplex_tree()
st_ex$insert(list(1:2, 2:5, 5:6, c(4,7), c(5,7), 6:7, 7:9))

test_that("'simplextree'-to-'igraph' conversion preserves vertices", {
  ig_ex <- as_igraph(st_ex)
  expect_equal(st_ex$n_simplices[[1L]], igraph::gorder(ig_ex))
  expect_true(all(st_ex$edges == sort_el(igraph::as_edgelist(ig_ex))))
})

test_that("'simplextree'-to-'network' conversion preserves vertices", {
  nw_ex <- as_network(st_ex)
  expect_equal(st_ex$n_simplices[[1L]], network::network.size(nw_ex))
  expect_true(all(st_ex$edges == sort_el(network::as.edgelist(nw_ex))))
})

test_that("'simplextree'-to-list conversion preserves 0,1-simplices", {
  cp_ex <- as_cmplx(st_ex)
  expect_equal(st_ex$n_simplices[[1L]], length(unique(unlist(cp_ex))))
  expect_true(all(st_ex$edges ==
                    t(sapply(cp_ex[sapply(cp_ex, length) == 2L], identity))))
})

st_gen <- lapply(st_ex$serialize(), `+`, 2L)
st_ex$clear()
st_ex$deserialize(st_gen)

test_that("'simplextree'-to-'igraph' conversion encodes indices", {
  ig_ex <- as_igraph(st_ex, index = "id")
  expect_true("id" %in% igraph::vertex_attr_names(ig_ex))
  expect_equal(st_ex$vertices, igraph::vertex_attr(ig_ex, "id"))
})

test_that("'simplextree'-to-'network' conversion encodes indices", {
  nw_ex <- as_network(st_ex, index = "id")
  expect_true("id" %in% network::list.vertex.attributes(nw_ex))
  expect_equal(st_ex$vertices, network::get.vertex.attribute(nw_ex, "id"))
})
