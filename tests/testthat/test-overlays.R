test_that("rank selection removes self by ID and preserves distinct tied neighbors", {
  nn <- list(
    idx = rbind(c(2, 1, 2, 3), c(2, 2, 3, 1), c(3, 1, 2, 2), rep(4, 4)),
    dist = rbind(c(0, 0, 0, 2), c(0, 0, 1, 2), c(0, 1, 1, 1), rep(0, 4))
  )
  all <- embed_edges(nn)
  expect_identical(all$from, rep(1:3, each = 2))
  expect_identical(all$to, c(2L, 3L, 3L, 1L, 1L, 2L))
  expect_identical(all$rank, rep(1:2, 3))
  expect_equal(all$distance, c(0, 2, 1, 2, 1, 1))
  expect_identical(attr(all, "n_vertices"), 4L)
  expect_identical(embed_edges(nn, "nearest")$to, c(2L, 3L, 1L))
  expect_identical(embed_edges(nn, "furthest")$to, c(3L, 1L, 2L))
  expect_identical(embed_edges(nn, k = 1)$to, embed_edges(nn, "nearest")$to)
  expect_error(embed_edges(nn, "furthest", 2), "Row 4")
  nn <- list(idx = nn$idx[1:3, ])
  expect_identical(embed_edges(nn, "furthest", 2)$rank, rep(2L, 3))
  expect_false("distance" %in% names(embed_edges(nn)))
  expect_identical(nrow(embed_edges(list(idx = matrix(1L)))), 0L)
})

test_that("invalid ranks, distances and indices are rejected", {
  nn <- list(idx = rbind(c(1, 2), c(2, 1)))
  expect_error(embed_edges(list()), "'idx'")
  expect_error(embed_edges(nn, "nearest", 1), "'k'")
  expect_error(embed_edges(nn, k = 1.5), "'k'")
  expect_error(embed_edges(list(idx = matrix(0))), "row IDs")
  expect_error(embed_edges(list(idx = matrix(1.5))), "row IDs")
  nn$dist <- matrix(1:2)
  expect_error(embed_edges(nn), "matching 'idx'")
  nn$dist <- rbind(c(2, 1), c(0, 1))
  expect_error(embed_edges(nn), "ranked in row 1")
})

test_that("sparse graphs provide unique undirected weights and retain isolates", {
  skip_if_not_installed("Matrix")
  g <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 1, 2),
    j = c(2, 3, 3, 4, 2),
    x = c(0.5, 0.2, 1, 0, 1),
    dims = c(4, 4),
    symmetric = TRUE
  )
  edges <- embed_edges(g)
  expect_identical(edges$from, 1:2)
  expect_identical(edges$to, 2:3)
  expect_equal(edges$weight, c(0.5, 0.2))
  expect_identical(attr(edges, "n_vertices"), 4L)
  expect_error(embed_edges(g, "nearest"), "Sparse graphs")
  expect_error(embed_edges(g, k = 1), "Sparse graphs")
  expect_error(embed_edges(Matrix::triu(g)), "symmetric")
  expect_identical(embed_edges(methods::as(g, "generalMatrix")), edges)
})

test_that("plotters validate endpoint IDs against original coordinate rows", {
  xy <- cbind(1:4, 4:1)
  expect_error(embed_plot(xy, overlays = list()), "edge table")
  expect_error(
    embed_plot(xy, overlays = data.frame(from = 0, to = 2)),
    "row IDs"
  )
  expect_error(
    embed_plot(xy, overlays = embed_edges(list(idx = matrix(1)))),
    "observation count"
  )
})
