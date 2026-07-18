#' Embedding Plot Using ggplot2
#'
#' Plots embedded coordinates with ggplot2 and returns an ordinary ggplot
#' object, so callers can add their own themes, annotations, guides, or other
#' ggplot layers.
#'
#' @inheritParams embed_plot
#' @param cex Size passed to the ggplot2 point or text layer.
#' @return A ggplot object.
#' @details
#' This is a small ggplot2 renderer for the same resolved coordinates and color
#' semantics as [embed_plot()]. It uses native ggplot2 discrete, continuous,
#' and identity scales. ggplot2 is an optional dependency and must be installed
#' to use this function.
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
#' embed_ggplot(pca_iris$x, iris$Species, title = "iris PCA")
embed_ggplot <- function(
  coords,
  x = NULL,
  colors = NULL,
  color_scheme = NULL,
  num_colors = 15,
  alpha_scale = 1,
  limits = NULL,
  top = NULL,
  cex = 1,
  title = NULL,
  text = NULL,
  sub = NULL,
  equal_axes = FALSE,
  pc_axes = FALSE,
  xlim = NULL,
  ylim = NULL,
  show_axes = TRUE,
  NA_color = NULL,
  rev = FALSE,
  verbose = FALSE
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use embed_ggplot().",
      call. = FALSE
    )
  }

  validate_logical_scalar(equal_axes, "'equal_axes'")
  validate_logical_scalar(pc_axes, "'pc_axes'")
  validate_logical_scalar(show_axes, "'show_axes'")
  validate_logical_scalar(rev, "'rev'")
  validate_logical_scalar(verbose, "'verbose'")
  validate_alpha_scale(alpha_scale)
  validate_cex(cex)
  coord_res <- prepare_coords(
    coords,
    pc_axes = pc_axes,
    xlim = xlim,
    ylim = ylim,
    equal_axes = equal_axes
  )
  coords <- coord_res$coords
  n <- nrow(coords)
  if (!is.null(text)) {
    text <- recycle_input(text, n, "'text'")
  }
  color_res <- resolve_colors(
    x = x,
    colors = colors,
    n = n,
    color_scheme = color_scheme,
    num_colors = num_colors,
    limits = limits,
    top = top,
    alpha_scale = alpha_scale,
    NA_color = NA_color,
    rev = rev,
    verbose = verbose
  )

  rows <- which(color_res$keep)
  data <- data.frame(
    x = coords[rows, 1],
    y = coords[rows, 2],
    color = color_res$colors[rows],
    stringsAsFactors = FALSE
  )
  if (!is.null(text)) {
    data$label <- text[rows]
  }

  if (color_res$kind == "discrete") {
    data$color <- factor(
      as.character(color_res$labels[rows]),
      levels = names(color_res$palette)
    )
    color_scale <- ggplot2::scale_color_manual(
      values = color_res$palette,
      na.value = if (is.null(NA_color)) NA else NA_color
    )
  } else if (color_res$kind == "continuous") {
    data$color <- color_res$mapped_values[rows]
    color_scale <- ggplot2::scale_color_gradientn(
      colors = color_res$palette,
      limits = color_res$limits,
      na.value = if (is.null(NA_color)) NA else NA_color
    )
  } else {
    color_scale <- ggplot2::scale_color_identity(guide = "none")
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[["x"]],
      y = .data[["y"]],
      color = .data[["color"]]
    )
  )
  if (is.null(text)) {
    p <- p + ggplot2::geom_point(size = cex, alpha = alpha_scale)
  } else {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = .data[["label"]]),
        size = cex,
        alpha = alpha_scale
      )
  }
  p <- p +
    color_scale +
    ggplot2::labs(
      title = title,
      subtitle = sub,
      x = if (show_axes) "X" else NULL,
      y = if (show_axes) "Y" else NULL
    )
  if (coord_res$fixed_aspect) {
    p <- p +
      ggplot2::coord_fixed(
        ratio = 1,
        xlim = coord_res$xlim,
        ylim = coord_res$ylim
      )
  } else {
    p <- p +
      ggplot2::coord_cartesian(
        xlim = coord_res$xlim,
        ylim = coord_res$ylim
      )
  }
  if (!show_axes) {
    p <- p +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }
  p
}
