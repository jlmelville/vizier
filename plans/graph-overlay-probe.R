# Run from the checkout with Rscript plans/graph-overlay-probe.R.
# This measures the static Plotly figure. It does not time a browser.
pkgload::load_all(".", quiet = TRUE)
set.seed(65)
fit <- uwot::umap(
  as.matrix(iris[1:4]),
  n_neighbors = 15,
  n_threads = 1,
  ret_nn = TRUE,
  ret_extra = "fgraph"
)
xy <- fit$embedding
ids <- seq_len(nrow(xy))
triangle <- Matrix::summary(Matrix::triu(Matrix::drop0(fit$fgraph), k = 1))
edges <- data.frame(from = triangle$i, to = triangle$j, weight = triangle$x)
g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = ids)
ends <- igraph::ends(g, igraph::E(g), names = FALSE)
paths <- edgebundle::edge_bundle_hammer(g, xy)
first <- !duplicated(paths$group)
last <- !duplicated(paths$group, fromLast = TRUE)
matches_xy <- function(path_rows, vertex_ids) {
  path_xy <- as.matrix(paths[path_rows, c("x", "y")])
  isTRUE(all.equal(
    path_xy,
    xy[vertex_ids, , drop = FALSE],
    check.attributes = FALSE
  ))
}
stopifnot(
  matches_xy(first, ends[paths$group[first], 1]),
  matches_xy(last, ends[paths$group[last], 2]),
  all(igraph::edge_attr(g, "weight") == fit$fgraph[ends])
)

# Count bridges using endpoint labels. Keep the two kNN choices directional.
nn <- fit$nn[[1]]$idx
choices <- lapply(ids, function(i) unique(nn[i, nn[i, ] != i]))
bridge_counts <- function(from, to) {
  crossing <- iris$Species[from] != iris$Species[to]
  pairs <- data.frame(a = pmin(from, to), b = pmax(from, to))
  c(edges = length(to), crossing_pairs = nrow(unique(pairs[crossing, ])))
}
print(rbind(
  nearest = bridge_counts(ids, vapply(choices, head, numeric(1), n = 1)),
  furthest = bridge_counts(ids, vapply(choices, tail, numeric(1), n = 1)),
  all_ranked = bridge_counts(rep(ids, lengths(choices)), unlist(choices)),
  fitted_graph = bridge_counts(edges$from, edges$to)
))

# A run of vertices per edge, with one NA between successive runs.
line_args <- function(x, y, group, width = 1) {
  position <- seq_along(group) + cumsum(c(FALSE, diff(group) != 0))
  xx <- yy <- rep(NA_real_, max(position))
  xx[position] <- x
  yy[position] <- y
  list(
    x = xx,
    y = yy,
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    hoverinfo = "skip",
    showlegend = FALSE,
    opacity = 0.2,
    line = list(color = "grey", width = width)
  )
}
straight <- function(e, width = 1) {
  vertices <- as.vector(rbind(e$from, e$to))
  group <- rep(seq_len(nrow(e)), each = 2)
  line_args(xy[vertices, 1], xy[vertices, 2], group, width)
}

# Build the point widget normally so the edge trace cannot change its palette.
labels <- iris$Species
pal <- c(setosa = "#AA0001", versicolor = "#00BB02", virginica = "#0000CC")
reference <- embed_plotly(xy, labels, color_scheme = pal, tooltip = ids)
point_widget <- plotly::plotly_build(reference)
measure <- function(lines) {
  p <- point_widget
  p$x$data <- c(lines, point_widget$x$data)
  built <- plotly::plotly_build(p)$x$data
  point_traces <- length(lines) + seq_along(point_widget$x$data)
  stopifnot(identical(built[point_traces], point_widget$x$data))
  for (i in seq_along(lines)) {
    trace <- built[[i]]
    stopifnot(
      identical(as.numeric(trace$x), lines[[i]]$x),
      identical(as.numeric(trace$y), lines[[i]]$y),
      trace$mode == "lines",
      identical(trace$hoverinfo, "skip"),
      !trace$showlegend
    )
  }
  slots <- vapply(lines, function(trace) length(trace$x), integer(1))
  json <- plotly::plotly_json(p, pretty = FALSE)
  c(
    line_traces = length(lines),
    xy_slots_per_axis = sum(slots),
    figure_json_bytes = nchar(json, type = "bytes")
  )
}
bins <- cut(
  edges$weight,
  seq(0, 1, 0.25),
  include.lowest = TRUE,
  labels = FALSE
)
batches <- lapply(sort(unique(bins)), function(i) {
  straight(edges[bins == i, ], width = i)
})
by_weight <- split(edges, match(edges$weight, unique(edges$weight)))
exact <- lapply(by_weight, function(e) straight(e, width = 1 + 3 * e$weight[1]))
incident <- edges$from == 1 | edges$to == 1
print(rbind(
  points_only = measure(list()),
  row_1 = measure(list(straight(edges[incident, ]))),
  all_straight = measure(list(straight(edges))),
  four_widths = measure(batches),
  exact_widths = measure(unname(exact)),
  all_hammer = measure(list(line_args(paths$x, paths$y, paths$group)))
))
