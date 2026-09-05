test_that("numeric palette sizes NULL, one, and two work in every renderer", {
  coords <- cbind(1:3, 4:6)
  for (values in list(c(1, 2, 3), c(5, 5, 5), rep(NA_real_, 3))) {
    for (size in list(NULL, 1, 2)) {
      args <- list(coords = coords, x = values, num_colors = size)
      colors <- do.call(base_colors, args)
      expect_length(colors, 3)
      if (all(is.finite(values))) {
        expect_false(anyNA(colors))
        if (identical(size, 1)) expect_length(unique(colors), 1)
      }
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        built <- ggplot2::ggplot_build(do.call(embed_ggplot, args))
        expect_equal(nrow(built$data[[1]]), 3)
      }
      if (requireNamespace("plotly", quietly = TRUE)) {
        built <- plotly::plotly_build(do.call(embed_plotly, args))
        expect_length(built$x$data[[1]]$x, 3)
      }
    }
  }
})

test_that("metadata inference skips empty colors and unsuitable character columns", {
  coords <- cbind(1:4, 5:8)
  groups <- c("a", "b", "a", "b")
  palette <- c("red", "blue")
  for (missing in list(rep(NA_character_, 4), rep(NA, 4))) {
    meta <- data.frame(group = factor(groups), empty = missing)
    expect_identical(
      base_colors(coords, meta, color_scheme = palette),
      grDevices::adjustcolor(palette[c(1, 2, 1, 2)])
    )
  }
  meta <- data.frame(group = groups, id = paste0("row", 1:4))
  expect_identical(
    base_colors(coords, meta, color_scheme = palette),
    grDevices::adjustcolor(palette[c(1, 2, 1, 2)])
  )
  meta$color <- c("red", NA, "blue", "red")
  expect_identical(
    base_colors(coords, meta),
    grDevices::adjustcolor(meta$color)
  )
})

test_that("named palettes require only observed levels and generated palettes stay stable", {
  coords <- cbind(1:3, 4:6)
  labels <- factor(c("C", "A", "C"), levels = c("A", "B", "C"))
  palette <- c(A = "red", C = "blue")
  expect_identical(
    base_colors(coords, labels, color_scheme = palette),
    grDevices::adjustcolor(c("blue", "red", "blue"))
  )
  expect_identical(
    base_colors(coords, labels, color_scheme = palette, rev = TRUE),
    grDevices::adjustcolor(c("red", "blue", "red"))
  )
  expect_error(
    base_colors(coords, labels, color_scheme = c(A = "red")),
    "missing observed categories: C"
  )
  expect_identical(
    base_colors(coords, labels, color_scheme = c("red", "green", "blue")),
    grDevices::adjustcolor(c("blue", "red", "blue"))
  )
})

# Inspect final graphical parameters, since layer alpha can overwrite hex alpha.
grob_colors <- function(grob, type) {
  result <- if (inherits(grob, type)) grob$gp$col else character()
  for (child in c(grob$grobs, grob$children)) {
    result <- c(result, grob_colors(child, type))
  }
  result
}

test_that("ggplot multiplies embedded alpha for points, text, missing colors and guides", {
  skip_if_not_installed("ggplot2")
  render <- function(p) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    ggplot2::ggplotGrob(p)
  }
  coords <- cbind(1:3, 4:6)
  for (labels in list(NULL, letters[1:3])) {
    for (alpha in c(1, 0.5, 0)) {
      cases <- list(
        list(colors = rep("#FF000040", 3)),
        list(
          x = factor(c("a", "b", NA)),
          color_scheme = c(a = "#FF000040", b = "#0000FF40")
        ),
        list(
          x = c(0, 0.5, NA),
          num_colors = 2,
          limits = c(0, 1),
          color_scheme = c("#FF000040", "#0000FF40")
        ),
        list(
          x = c(0, 0.5, NA),
          limits = c(0, 1),
          color_scheme = c("#FF000040", "#0000FF40")
        )
      )
      for (args in cases) {
        p <- do.call(
          embed_ggplot,
          c(
            list(
              coords = coords,
              text = labels,
              alpha_scale = alpha,
              NA_color = "#00FF0040"
            ),
            args
          )
        )
        grob <- render(p)
        panel <- grob$grobs[[which(grob$layout$name == "panel")]]
        colors <- grob_colors(panel, if (is.null(labels)) "points" else "text")
        expect_length(colors, 3)
        expect_equal(
          unname(grDevices::col2rgb(colors, alpha = TRUE)[4, ]),
          rep(round(64 * alpha), 3),
          tolerance = 1 / 255
        )
        if (is.null(labels) && is.factor(args$x)) {
          keys <- grob_colors(grob, "points")
          expect_gt(length(keys), 3)
          expect_equal(
            unname(grDevices::col2rgb(keys, alpha = TRUE)[4, ]),
            rep(round(64 * alpha), length(keys)),
            tolerance = 1 / 255
          )
        }
      }
    }
  }
})

test_that("Plotly text applies alpha in all color branches", {
  skip_if_not_installed("plotly")
  coords <- cbind(1:3, 4:6)
  for (alpha in c(1, 0.5, 0)) {
    for (args in list(
      list(colors = "#FF000040"),
      list(x = 1:3),
      list(x = factor(c("a", "b", "a"))),
      list()
    )) {
      p <- do.call(
        embed_plotly,
        c(list(coords = coords, text = letters[1:3], alpha_scale = alpha), args)
      )
      traces <- plotly::plotly_build(p)$x$data
      for (trace in traces) {
        expect_identical(trace$mode, "text")
        expect_equal(trace$opacity, alpha)
        if (!is.null(args$colors)) {
          expect_equal(as.vector(trace$textfont$color), rep("#FF000040", 3))
        }
      }
    }
  }
})

test_that("unused factor levels are absent from backend legends", {
  labels <- factor(c("C", "A", "C"), levels = c("A", "B", "C"))
  args <- list(
    coords = cbind(1:3, 4:6),
    x = labels,
    color_scheme = c(A = "red", C = "blue")
  )
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    built <- ggplot2::ggplot_build(do.call(embed_ggplot, args))
    expect_equal(
      built$plot$scales$get_scales("colour")$get_breaks(),
      c("A", "C"),
      ignore_attr = TRUE
    )
  }
  if (requireNamespace("plotly", quietly = TRUE)) {
    traces <- plotly::plotly_build(do.call(embed_plotly, args))$x$data
    expect_equal(vapply(traces, `[[`, character(1), "name"), c("A", "C"))
  }
})
