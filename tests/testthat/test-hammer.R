hammer_fixture <- function() {
  list(
    xy = cbind(c(1, 4, 0, 3, 7, 5, 2), c(2, 0, 4, 5, 1, 3, 8)),
    spec = list(
      edges = data.frame(
        from = c(3, 2, 5, 4, 2, 1, 1),
        to = c(3, 6, 3, 1, 1, 4, 5),
        weight = c(1, 0.02, 0.26, 0.52, 0.73, 0.1, 0.95)
      ),
      route = "hammer",
      alpha = "weight",
      alpha_limits = c(0, 1),
      alpha_range = c(0.01, 0.25)
    )
  )
}

test_that("one Hammer preparation supplies all three renderers", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  fixture <- hammer_fixture()
  xy <- fixture$xy
  spec <- fixture$spec
  original <- vizier:::hammer_paths
  calls <- 0L
  local_mocked_bindings(
    hammer_paths = function(...) {
      calls <<- calls + 1L
      original(...)
    },
    .package = "vizier"
  )
  hammer <- prepare_overlay(xy, spec, pc_axes = TRUE)
  ids <- c(2L, 3L, 4L, 5L, 7L)
  expect_identical(hammer$edges$edge_id, ids)
  expect_equal(hammer$edges$weight, spec$edges$weight[ids])
  alpha <- 0.01 + 0.24 * spec$edges$weight[ids]
  rotated <- vizier:::pc_rotate(xy)
  paths <- lapply(ids, function(id) {
    path <- hammer$paths[hammer$paths$edge_id == id, c("x", "y")]
    endpoints <- unname(as.matrix(path[c(1L, nrow(path)), ]))
    rows <- c(spec$edges$from[id], spec$edges$to[id])
    forward <- rotated[rows, , drop = FALSE]
    expect_true(
      min(max(abs(endpoints - forward)), max(abs(endpoints - forward[2:1, ]))) <
        1e-8
    )
    path
  })

  strokes <- list()
  local_mocked_bindings(
    lines = function(x, y, col) {
      strokes[[length(strokes) + 1L]] <<- list(x = x, y = y, col = col)
    },
    .package = "graphics"
  )
  score <- seq_len(nrow(xy))
  base_plot_args(xy, score, overlays = hammer, pc_axes = TRUE)
  expect_length(strokes, length(ids))
  for (i in seq_along(paths)) {
    expect_equal(strokes[[i]]$x, paths[[i]]$x)
    expect_equal(strokes[[i]]$y, paths[[i]]$y)
    expect_equal(
      unname(grDevices::col2rgb(strokes[[i]]$col, alpha = TRUE)[4]),
      round(255 * alpha[i])
    )
  }
  p <- embed_ggplot(xy, score, overlays = hammer, pc_axes = TRUE)
  layer <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(layer$x, hammer$paths$x)
  expect_equal(layer$y, hammer$paths$y)
  expect_equal(as.numeric(layer$alpha), alpha[match(hammer$paths$edge_id, ids)])

  binned <- hammer
  binned$alpha_bins <- 4L
  widget <- embed_plotly(xy, score, overlays = binned, pc_axes = TRUE)
  traces <- plotly::plotly_build(widget)$x$data
  points <- plotly::plotly_build(embed_plotly(xy, score, pc_axes = TRUE))
  expect_identical(traces[-(1:4)], points$x$data)
  colors <- vapply(traces[1:4], function(trace) trace$line$color, "")
  levels <- c(0.01, 0.09, 0.17, 0.25)
  bin <- c(1L, 2L, 3L, 3L, 4L)
  for (i in seq_along(levels)) {
    trace <- traces[[match(paste0("rgba(64,64,64,", levels[i], ")"), colors)]]
    expect_identical(trace$mode, "lines")
    expect_identical(trace$hoverinfo, "skip")
    expect_false(trace$showlegend)
    expect_false(trace$connectgaps)
    for (axis in c("x", "y")) {
      expected <- unlist(
        lapply(paths[bin == i], function(path) {
          c(path[[axis]], NA_real_)
        }),
        use.names = FALSE
      )
      expect_equal(as.numeric(trace[[axis]]), head(expected, -1L))
    }
  }
  expect_identical(binned$paths, hammer$paths)
  expect_null(hammer$alpha_bins)
  expect_identical(calls, 1L)
})

test_that("inline routing, display filtering and coordinate checks agree", {
  skip_if_not_installed("ggplot2")
  fixture <- hammer_fixture()
  xy <- fixture$xy
  spec <- fixture$spec
  hammer <- prepare_overlay(xy, spec)
  # Rows one and four have the two highest scores.
  scores <- c(7, 1, 2, 6, 3, 4, 5)
  p <- embed_ggplot(xy, scores, top = 2, overlays = spec)
  expect_equal(
    p$layers[[1]]$data$edge_id,
    hammer$paths$edge_id[hammer$paths$edge_id == 4L]
  )
  coords <- as.data.frame(xy)
  p <- embed_ggplot(coords, scores, top = 2, overlays = hammer)
  expect_equal(p$layers[[1]]$data$x, hammer$paths$x[hammer$paths$edge_id == 4L])
  expect_error(embed_ggplot(xy[-1, ], overlays = hammer), "coordinates")
  expect_error(embed_ggplot(xy[7:1, ], overlays = hammer), "coordinates")
  expect_error(embed_ggplot(xy, overlays = hammer, pc_axes = TRUE), "pc_axes")
  expect_length(embed_ggplot(xy, scores, top = 1, overlays = hammer)$layers, 1L)
})

test_that("weight mapping validates, clips and rounds at draw time", {
  fixture <- hammer_fixture()
  spec <- fixture$spec
  spec$edges <- data.frame(from = integer(), to = integer(), weight = numeric())
  hammer <- prepare_overlay(fixture$xy, spec)
  expect_equal(nrow(hammer$paths), 0L)
  spec$edges$weight <- NULL
  expect_error(prepare_overlay(fixture$xy, spec), "weight")
  spec$alpha <- NULL
  expect_equal(nrow(prepare_overlay(fixture$xy, spec)$edges), 0L)
  expect_error(prepare_overlay(fixture$xy, list()), "route")
  spec$invalid <- TRUE
  expect_error(prepare_overlay(fixture$xy, spec), "Unknown")
  style <- list(
    edges = data.frame(weight = c(-1, 0, 0.55, 1, 2)),
    alpha = "weight"
  )
  expect_equal(vizier:::overlay_alpha(style), c(0.01, 0.01, 0.142, 0.25, 0.25))
  style$alpha_limits <- c(0, 2)
  style$alpha_range <- c(0.2, 0.8)
  style$alpha_bins <- 3L
  expect_equal(vizier:::overlay_alpha(style), c(0.2, 0.2, 0.5, 0.5, 0.8))
  style$alpha_bins <- 1.5
  expect_error(vizier:::overlay_alpha(style), "alpha_bins")
  style$alpha_range <- c(0, 2)
  expect_error(vizier:::overlay_alpha(style), "alpha_range")
  style$alpha_limits <- c(1, 1)
  expect_error(vizier:::overlay_alpha(style), "alpha_limits")
})

test_that("native Hammer paths match the reference at every thread count", {
  fixture <- hammer_fixture()
  route <- function(threads) {
    spec <- fixture$spec
    spec$n_threads <- threads
    prepare_overlay(fixture$xy, spec)$paths
  }
  serial <- route(0L)
  expect_identical(route(1L), serial)
  expect_identical(route(3L), serial)
  expect_equal(nrow(serial), 250L)
  # Vertex 25 in each path, from edgebundle 1.0.1 with its default settings.
  mid <- seq(25L, nrow(serial), by = 50L)
  expect_equal(
    serial$x[mid],
    c(4.27667640, 3.14069397, 1.93087738, 2.56869634, 3.87557543),
    tolerance = 1e-7
  )
  expect_equal(
    serial$y[mid],
    c(1.36540118, 1.98670796, 3.45156685, 1.12329877, 1.15846022),
    tolerance = 1e-7
  )
  for (bad in list(-1, 1.5, NA_real_, Inf, "4", c(1, 2), 2^31)) {
    expect_error(route(bad), "n_threads")
  }
})

test_that("Hammer routes a graph with coincident integer coordinates", {
  xy <- matrix(7L, nrow = 3L, ncol = 2L)
  spec <- list(
    edges = data.frame(from = 3L, to = 1L),
    route = "hammer",
    n_threads = 4L
  )
  paths <- prepare_overlay(xy, spec)$paths
  expect_true(all(is.finite(paths$x)) && all(is.finite(paths$y)))
  expect_equal(paths$x[c(1L, 50L)], c(7, 7))
  expect_equal(paths$y[c(1L, 50L)], c(7, 7))
  expect_identical(paths$edge_id, rep(1L, 50))
})
