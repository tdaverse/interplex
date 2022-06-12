context("Python GUDHI simplex trees")

gd <- reticulate::import("gudhi")
gd_ex <- gd$SimplexTree()
rm(gd)
for (s in list(1:2, 2:5, 5:6, c(4,7), c(5,7), 6:7, 7:9))
  gd_ex$insert(as.list(s))
gd_el <- py_gudhi_edgelist(gd_ex)

test_that("GUDHI-to-'igraph' conversion preserves 0,1-simplices", {
  ig_ex <- as_igraph(gd_ex)
  expect_equal(gd_ex$num_vertices(), igraph::gorder(ig_ex))
  expect_true(all(gd_el == sort_el(igraph::as_edgelist(ig_ex))))
})

test_that("GUDHI-to-'network' conversion preserves vertices", {
  nw_ex <- as_network(gd_ex)
  expect_equal(gd_ex$num_vertices(), network::network.size(nw_ex))
  expect_true(all(gd_el == sort_el(network::as.edgelist(nw_ex))))
})

test_that("GUDHI-to-'simplextree' conversion preserves all simplices", {
  st_ex <- as_simplextree(gd_ex)
  expect_equal(gd_ex$num_vertices(), st_ex$n_simplices[[1L]])
  expect_identical(
    sort_lst(reticulate::iterate(gd_ex$get_simplices(), function(s) s[[1L]])),
    sort_lst(simplextree_list(st_ex))
  )
})

test_that("GUDHI-to-list conversion preserves 0,1-simplices", {
  cp_ex <- as_cmplx(gd_ex)
  expect_equal(gd_ex$num_vertices(), length(unique(unlist(cp_ex))))
  expect_true(all(gd_el ==
                    t(sapply(cp_ex[sapply(cp_ex, length) == 2L], identity))))
})

gd <- reticulate::import("gudhi")
gd_ex <- gd$SimplexTree()
rm(gd)
for (s in list(3:5, c(5, 8, 13), c(3, 13), 13:14, 14:15))
  gd_ex$insert(as.list(s))
gd_el <- py_gudhi_edgelist(gd_ex)

test_that("GUDHI-to-'igraph' conversion encodes indices", {
  ig_ex <- as_igraph(gd_ex, index = "id")
  expect_true("id" %in% igraph::vertex_attr_names(ig_ex))
  expect_equal(
    reticulate::iterate(gd_ex$get_skeleton(0L), function(s) s[[1L]]),
    igraph::vertex_attr(ig_ex, "id")
  )
})

test_that("GUDHI-to-'network' conversion encodes indices", {
  nw_ex <- as_network(gd_ex, index = "id")
  expect_true("id" %in% network::list.vertex.attributes(nw_ex))
  expect_equal(
    reticulate::iterate(gd_ex$get_skeleton(0L), function(s) s[[1L]]),
    network::get.vertex.attribute(nw_ex, "id")
  )
})
