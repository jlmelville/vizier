test_that("embed_plotly accepts paletteer color vectors", {
  testthat::skip_if_not_installed("plotly")

  pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)

  p <- embed_plotly(
    pca_iris$x,
    iris,
    color_scheme = "jcolors::rainbow::c",
    cex = 2,
    title = "jcolors rainbow (continuous)"
  )

  expect_s3_class(p, "plotly")
  expect_s3_class(plotly::plotly_build(p), "plotly")
})

test_that("embed_plotly builds representative plotly widgets", {
  testthat::skip_if_not_installed("plotly")

  pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
  idx <- c(1:5, 51:55, 101:105)
  coords <- pca_iris$x[idx, ]
  species <- droplevels(iris$Species[idx])

  cases <- list(
    factor = embed_plotly(
      coords,
      species,
      color_scheme = c("#AA0000", "#0055AA", "#118833")
    ),
    character = embed_plotly(
      coords,
      as.character(species),
      color_scheme = c("#AA0000", "#0055AA", "#118833")
    ),
    numeric = embed_plotly(
      coords,
      iris$Petal.Length[idx],
      color_scheme = "RColorBrewer::Blues"
    ),
    numeric_limits = embed_plotly(
      coords,
      iris$Petal.Length[idx],
      color_scheme = "RColorBrewer::Blues",
      limits = c(2, 5)
    ),
    explicit_colors = embed_plotly(
      coords,
      colors = rep(c("#AA0000", "#0055AA", "#118833"), length.out = length(idx))
    ),
    text_labels = embed_plotly(
      coords,
      species,
      color_scheme = c("#AA0000", "#0055AA", "#118833"),
      text = as.character(species)
    ),
    custom_tooltips = embed_plotly(
      coords,
      species,
      color_scheme = c("#AA0000", "#0055AA", "#118833"),
      tooltip = paste("Species:", species)
    )
  )

  for (p in cases) {
    expect_s3_class(p, "plotly")
    expect_s3_class(plotly::plotly_build(p), "plotly")
  }
})

test_that("embed_plotly handles numeric limit clipping explicitly", {
  testthat::skip_if_not_installed("plotly")

  coords <- cbind(seq_len(5), seq_len(5))
  values <- c(1, 2, 3, 4, 5)

  clipped <- embed_plotly(
    coords,
    values,
    color_scheme = c("#AA0000", "#0055AA"),
    limits = c(2, 4)
  )
  clipped_trace <- plotly::plotly_build(clipped)$x$data[[1]]

  expect_equal(
    as.numeric(clipped_trace$marker$color),
    c(2, 2, 3, 4, 4)
  )
  expect_equal(
    as.character(clipped_trace$text),
    paste0(seq_along(values), ": ", values)
  )

  unclipped <- embed_plotly(
    coords,
    values,
    color_scheme = c("#AA0000", "#0055AA"),
    limits = c(2, 4),
    clip_limit_values = FALSE
  )
  unclipped_trace <- plotly::plotly_build(unclipped)$x$data[[1]]

  expect_equal(
    as.numeric(unclipped_trace$marker$color),
    c(NA, 2, 3, 4, NA)
  )
})

test_that("embed_plotly preserves list coordinates and explicit ranges", {
  testthat::skip_if_not_installed("plotly")

  coords <- cbind(c(0, 2, 4), c(10, 20, 30))

  p <- embed_plotly(
    list(coords = coords),
    colors = "#AA0000",
    xlim = c(-1, 5),
    ylim = c(9, 31)
  )
  built <- plotly::plotly_build(p)
  trace <- built$x$data[[1]]

  expect_equal(as.numeric(trace$x), coords[, 1])
  expect_equal(as.numeric(trace$y), coords[, 2])
  expect_equal(built$x$layout$xaxis$range, c(-1, 5))
  expect_equal(built$x$layout$yaxis$range, c(9, 31))
})

test_that("embed_plotly equal_axes uses a shared, fixed coordinate system", {
  testthat::skip_if_not_installed("plotly")

  coords <- cbind(c(0, 2, 4), c(10, 20, 30))

  p <- embed_plotly(coords, colors = "#AA0000", equal_axes = TRUE)
  built <- plotly::plotly_build(p)

  expect_equal(built$x$layout$xaxis$range, range(coords))
  expect_equal(built$x$layout$yaxis$range, range(coords))
  expect_identical(built$x$layout$xaxis$scaleanchor, "y")
  expect_identical(built$x$layout$xaxis$scaleratio, 1)
})

test_that("embed_plotly pc_axes writes rotated coordinates to the trace", {
  testthat::skip_if_not_installed("plotly")

  coords <- cbind(
    x = c(0, 1, 4, 6),
    y = c(1, 3, 2, 8)
  )
  rotated <- vizier:::pc_rotate(coords)

  p <- embed_plotly(coords, colors = "#AA0000", pc_axes = TRUE)
  trace <- plotly::plotly_build(p)$x$data[[1]]

  expect_equal(as.numeric(trace$x), rotated[, 1])
  expect_equal(as.numeric(trace$y), rotated[, 2])
})

test_that("embed_plotly exposes direct marker and text colors", {
  testthat::skip_if_not_installed("plotly")

  coords <- cbind(c(0, 2, 4), c(10, 20, 30))
  colors <- c("#AA0000", "#0055AA", "#118833")

  markers <- plotly::plotly_build(embed_plotly(
    coords,
    color_scheme = colors
  ))
  marker_trace <- markers$x$data[[1]]

  expect_identical(marker_trace$mode, "markers")
  expect_equal(as.character(marker_trace$marker$color), colors)
  expect_false(markers$x$layout$showlegend)

  text <- plotly::plotly_build(embed_plotly(
    coords,
    colors = colors,
    text = c("a", "b", "c")
  ))
  text_trace <- text$x$data[[1]]

  expect_identical(text_trace$mode, "text")
  expect_equal(as.character(text_trace$text), c("a", "b", "c"))
  expect_equal(as.character(text_trace$textfont$color), colors)
  expect_equal(as.character(text_trace$hovertext), c("1: a", "2: b", "3: c"))
})

test_that("plotly helpers handle scalar color scales and empty hover labels", {
  colorscale <- vizier:::plotly_colorscale("#AA0000")

  expect_equal(colorscale, list(list(0, "#AA0000"), list(1, "#AA0000")))
  expect_equal(vizier:::plotly_hover_text(3), c("1: ", "2: ", "3: "))
})

test_that("embed_plotly uses the resolved Viridis scale and numeric text mode", {
  testthat::skip_if_not_installed("plotly")

  coords <- cbind(1:3, 4:6)
  values <- c(1, 2, 3)
  built <- plotly::plotly_build(embed_plotly(coords, values))
  trace <- built$x$data[[1]]
  text_built <- plotly::plotly_build(embed_plotly(
    coords,
    values,
    text = c("one", "two", "three")
  ))
  text_trace <- text_built$x$data[[1]]

  expect_equal(
    vapply(trace$marker$colorscale, `[[`, character(1), 2),
    grDevices::hcl.colors(15, palette = "Viridis")
  )
  expect_identical(text_trace$mode, "text")
  expect_equal(as.character(text_trace$text), c("one", "two", "three"))
  expect_equal(
    as.character(text_trace$textfont$color),
    vizier:::resolve_colors(values, NULL, n = 3)$colors
  )
  expect_equal(
    as.character(text_trace$hovertext),
    c("1: one", "2: two", "3: three")
  )
})

test_that("embed_plotly keeps identity colors out of default hover content", {
  testthat::skip_if_not_installed("plotly")

  built <- plotly::plotly_build(embed_plotly(
    cbind(1:2, 3:4),
    colors = c("#AA0000", "#0055AA")
  ))
  trace <- built$x$data[[1]]

  expect_equal(as.character(trace$hovertext), c("1: ", "2: "))
})

test_that("embed_plotly validates tooltip length at its public boundary", {
  testthat::skip_if_not_installed("plotly")

  expect_error(
    embed_plotly(cbind(1:2, 3:4), tooltip = letters[1:3]),
    "length 1"
  )
})
