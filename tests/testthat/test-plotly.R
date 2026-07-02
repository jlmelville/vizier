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
