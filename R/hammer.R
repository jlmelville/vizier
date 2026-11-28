#' Prepare Hammer Paths for Reuse Across Embedding Plots
#'
#' Bundle a graph once with Hammer edge bundling. Pass the result to the
#' `overlays` argument of [embed_plot()], [embed_ggplot()] or
#' [embed_plotly()]. All three also accept an inline specification, which
#' computes the paths for that call.
#'
#' @inheritParams embed_plot
#' @param overlay A list with `edges` (an edge table as in [embed_edges()]) and
#'   `route = "hammer"`. Self-loops are omitted. Each undirected pair keeps
#'   its first row and weight. The default stroke has opacity 0.25.
#'   Set `alpha = "weight"` to map the numeric `weight` column linearly from
#'   `alpha_limits` (default `c(0, 1)`) to `alpha_range` (default
#'   `c(0.01, 0.25)`). Weights outside the limits use the nearest endpoint.
#'   Opacity is continuous unless `alpha_bins` is set to an integer of at
#'   least two. This rounds to the nearest equally spaced opacity level,
#'   including both endpoints, with ties rounded up. Set `n_threads` to the
#'   maximum number of routing threads. The default of 0 and a value of 1
#'   both run serially. Thread count does not affect the resulting paths.
#' @return A list of class `vizier_overlay` with the normalized `edges`,
#'   ordered `paths` (columns `x`, `y` and `edge_id`), resolved coordinates,
#'   axis setting and editable opacity settings. The `edge_id` in both tables
#'   identifies the original input edge row.
#' @details The native router samples each edge at 50 points and moves
#'   them toward shared density ridges over 12 iterations. Membership weights
#'   affect opacity. Drawing the result reuses the stored paths.
#'   Use the same coordinate rows and `pc_axes` setting in each plot.
#'   A display filter such as `top` hides whole paths.
#'   To route a different graph, subset the edges before preparing.
#'
#'   Plotly uses one line trace per distinct displayed opacity. Set
#'   `alpha_bins = 4L` on a copy to limit that widget to four edge traces.
#' @export
#' @examples
#' xy <- as.matrix(iris[1:8, c("Sepal.Length", "Sepal.Width")])
#' edges <- data.frame(from = 1:4, to = 5:8, weight = c(0.1, 0.4, 0.6, 1))
#' hammer <- prepare_overlay(xy, list(
#'   edges = edges, route = "hammer", alpha = "weight"
#' ))
#' embed_plot(xy, colors = "navy", overlays = hammer)
#' binned <- hammer
#' binned$alpha_bins <- 4L
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   embed_plotly(xy, colors = "navy", overlays = binned)
#' }
prepare_overlay <- function(coords, overlay, pc_axes = FALSE) {
  validate_logical_scalar(pc_axes, "'pc_axes'")
  coords <- prepare_coords(coords, pc_axes = pc_axes)$coords
  prepare_hammer(coords, overlay, pc_axes)
}

prepare_hammer <- function(coords, overlay, pc_axes) {
  if (
    !is.list(overlay) ||
      !identical(overlay[["route"]], "hammer") ||
      !is.data.frame(overlay[["edges"]])
  ) {
    stop("Supply list(edges = table, route = \"hammer\").", call. = FALSE)
  }
  fields <- c(
    "edges",
    "route",
    "n_threads",
    "alpha",
    "alpha_limits",
    "alpha_range",
    "alpha_bins"
  )
  unknown <- setdiff(names(overlay), fields)
  if (length(unknown)) {
    stop("Unknown Hammer overlay field: ", unknown[1L], call. = FALSE)
  }
  input <- overlay$edges
  edges <- overlay_pairs(input, nrow(coords), rep(TRUE, nrow(coords)))
  if (!is.null(input[["weight"]])) {
    edges$weight <- input[edges$edge_id, , drop = FALSE][["weight"]]
  }
  overlay$edges <- edges
  overlay_alpha(overlay)
  overlay$coords <- coords
  overlay$pc_axes <- pc_axes
  n_threads <- overlay[["n_threads"]]
  if (is.null(n_threads)) {
    n_threads <- 0L
  }
  if (
    !is.numeric(n_threads) ||
      length(n_threads) != 1L ||
      !is.null(dim(n_threads)) ||
      !is.finite(n_threads) ||
      n_threads < 0 ||
      n_threads > .Machine$integer.max ||
      n_threads != floor(n_threads)
  ) {
    stop("'n_threads' must be a nonnegative integer.", call. = FALSE)
  }
  overlay$paths <- hammer_paths(
    coords,
    edges$from,
    edges$to,
    edges$edge_id,
    as.integer(n_threads)
  )
  structure(overlay, class = "vizier_overlay")
}

overlay_alpha <- function(overlay) {
  edges <- overlay$edges
  alpha <- overlay[["alpha"]]
  if (is.null(alpha)) {
    return(rep(0.25, nrow(edges)))
  }
  if (
    !identical(alpha, "weight") ||
      !is.numeric(edges$weight) ||
      !is.null(dim(edges$weight)) ||
      any(!is.finite(edges$weight))
  ) {
    stop(
      "Use alpha = \"weight\" with a finite numeric weight column.",
      call. = FALSE
    )
  }
  limits <- if (is.null(overlay[["alpha_limits"]])) {
    c(0, 1)
  } else {
    overlay$alpha_limits
  }
  range <- if (is.null(overlay[["alpha_range"]])) {
    c(0.01, 0.25)
  } else {
    overlay$alpha_range
  }
  if (
    !is.numeric(limits) ||
      length(limits) != 2L ||
      any(!is.finite(limits)) ||
      limits[1] >= limits[2]
  ) {
    stop("'alpha_limits' must be two increasing finite numbers.", call. = FALSE)
  }
  if (
    !is.numeric(range) ||
      length(range) != 2L ||
      any(!is.finite(range)) ||
      range[1] > range[2] ||
      any(range < 0 | range > 1)
  ) {
    stop(
      "'alpha_range' must be two ordered opacities in [0, 1].",
      call. = FALSE
    )
  }
  t <- pmin(1, pmax(0, (edges$weight - limits[1]) / diff(limits)))
  bins <- overlay[["alpha_bins"]]
  if (!is.null(bins)) {
    if (
      !is.numeric(bins) ||
        length(bins) != 1L ||
        !is.finite(bins) ||
        bins < 2 ||
        bins != trunc(bins)
    ) {
      stop("'alpha_bins' must be an integer of at least two.", call. = FALSE)
    }
    t <- floor(t * (bins - 1) + 0.5) / (bins - 1)
  }
  range[1] + t * diff(range)
}

resolve_overlay <- function(overlays, coords, keep, pc_axes) {
  if (inherits(overlays, "vizier_overlay")) {
    if (
      !identical(pc_axes, overlays$pc_axes) ||
        !identical(dim(coords), dim(overlays$coords)) ||
        !all(coords == overlays$coords)
    ) {
      stop(
        "Use the coordinates and pc_axes setting used to prepare the overlay.",
        call. = FALSE
      )
    }
  } else if (
    is.list(overlays) &&
      !is.data.frame(overlays) &&
      identical(overlays[["route"]], "hammer")
  ) {
    overlays <- prepare_hammer(coords, overlays, pc_axes)
  } else {
    pairs <- overlay_pairs(overlays, nrow(coords), keep)
    if (NROW(pairs)) {
      return(list(pairs = pairs))
    }
    return(NULL)
  }
  edges <- overlays$edges
  visible <- keep[edges$from] & keep[edges$to]
  paths <- overlays$paths
  paths <- paths[paths$edge_id %in% edges$edge_id[visible], , drop = FALSE]
  alpha <- overlay_alpha(overlays)
  if (!nrow(paths)) {
    return(NULL)
  }
  paths$alpha <- alpha[match(paths$edge_id, edges$edge_id)]
  list(paths = paths)
}

# A stroke can contain many paths at the same opacity. Break it between edges.
path_batches <- function(paths) {
  opacities <- unique(paths$alpha)
  groups <- split(seq_len(nrow(paths)), match(paths$alpha, opacities))
  lapply(unname(groups), function(rows) {
    ids <- paths$edge_id[rows]
    pos <- seq_along(rows) + cumsum(c(FALSE, diff(ids) != 0))
    x <- y <- rep(NA_real_, pos[length(pos)])
    x[pos] <- paths$x[rows]
    y[pos] <- paths$y[rows]
    list(x = x, y = y, alpha = paths$alpha[rows[1L]])
  })
}
